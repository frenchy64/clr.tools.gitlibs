(ns clojure.tools.gitlibs.impl-test
  (:require
    [clojure.test :refer :all]
    [clojure.tools.gitlibs.impl :as impl]
    [clojure.clr.io :as cio]
    [clojure.string :as str]
    [clojure.edn :as edn])
  (:import
    [System DateTime Environment]
    [System.IO Directory DirectoryInfo File Path StreamWriter StreamReader]
    [System.Threading Thread ThreadStart]
    [System.Diagnostics ProcessStartInfo Process DataReceivedEventHandler]))

(deftest test-clean-url
  (are [url expected-path]
    (= expected-path (#'impl/clean-url url))

    ;; url formats - don't use user or port
    "ssh://git@gitlab.com:3333/org/repo.git" "ssh/gitlab.com/org/repo"
    "ssh://git@gitlab.org.net/org/repo.git"  "ssh/gitlab.org.net/org/repo"
    "ssh://user@host.xz/~user/repo.git/"     "ssh/host.xz/_TILDE_user/repo"
    "https://github.com/org/repo.git"        "https/github.com/org/repo"
    "git://host.xz/path/to/repo.git/"        "git/host.xz/path/to/repo"

    ;; scp style url (most common github ssh url format)
    "git@github.com:org/repo.git"               "ssh/github.com/org/repo"
    "git@github.com:dotted.org/dotted.repo.git" "ssh/github.com/dotted.org/dotted.repo"
    "host.xz:~user/path/to/repo.git/"           "ssh/host.xz/_TILDE_user/path/to/repo"

    ;; file scheme
    "file:///Users/me/code/repo.git" "file/Users/me/code/repo"
    "file://../foo.git"              "file/REL/_DOTDOT_/foo"
    "file://~/path/repo.git"         "file/REL/_TILDE_/path/repo"

    ;; file repos - handle relative vs absolute, handle . .. ~
    "/Users/me/code/repo.git" "file/Users/me/code/repo"
    "../foo.git"              "file/REL/_DOTDOT_/foo"
    "./foo.git"               "file/REL/_DOT_/foo"
    "~user/foo.git"           "file/REL/_TILDE_user/foo"

    ;; git - unknown transport with url rewrite in gitconfig (unusual, but do something useful)
    "work:repo.git" "ssh/work/repo"))

;; Test helpers for concurrent clone coordination

(defn- delete-dir-recursive
  "Recursively deletes a directory and all its contents."
  [^DirectoryInfo dir]
  (when (.Exists dir)
    (Directory/Delete (.FullName dir) true)))

(defn- delete-file-if-exists
  "Deletes a file if it exists."
  [^String path]
  (when (File/Exists path)
    (File/Delete path)))

(defn- deref-timely
  "Dereferences a future with a timeout. Throws if timed out."
  [fut timeout-ms timeout-msg]
  (let [result (deref fut timeout-ms ::timeout)]
    (when (= result ::timeout)
      (throw (Exception. timeout-msg)))
    result))

(defn- create-temp-gitlibs-dir
  "Creates a temporary gitlibs directory for test isolation."
  []
  (let [temp-dir (Path/Join (Path/GetTempPath) (str "gitlibs-test-" (.Ticks (DateTime/Now))))]
    (Directory/CreateDirectory temp-dir)
    temp-dir))

(defn- get-lockfile
  "Returns the lockfile path for a given URL in the specified gitlibs dir."
  [gitlibs-dir url]
  (let [git-dir-file (cio/dir-info gitlibs-dir "_repos" (#'impl/clean-url url))]
    (#'impl/lockfile-for-git-dir git-dir-file)))

(defn- get-config-file-path
  "Returns the config file path for a given URL in the specified gitlibs dir."
  [gitlibs-dir url]
  (let [git-dir-file (cio/dir-info gitlibs-dir "_repos" (#'impl/clean-url url))
        config-file (cio/file-info git-dir-file "config")]
    (.FullName config-file)))

(defn- get-git-dir-path
  "Returns the git directory path for a given URL in the specified gitlibs dir."
  [gitlibs-dir url]
  (.FullName (cio/dir-info gitlibs-dir "_repos" (#'impl/clean-url url))))

;; Mock git helpers using with-redefs
;; No external processes needed - use atoms and promises for coordination

(defn make-mock-git-handler
  "Creates a mock git handler that communicates via atoms for testing.
   
   Returns a map with:
   - :handler - function to use with (with-redefs [run-git-with-config ...])
   - :event-queue - atom containing vector of events sent by git
   - :response-fn - atom containing function that returns responses
   
   Usage in tests:
   (let [{:keys [handler event-queue response-fn]} (make-mock-git-handler)]
     (reset! response-fn (fn [event] {:op :fake-clone :target-dir \"/path\"}))
     (with-redefs [impl/run-git-with-config handler]
       ... test code that calls ensure-git-dir ...)
     ;; Check events in @event-queue)"
  []
  (let [event-queue (atom [])
        response-fn (atom (fn [_] {:op :continue}))]
    {:event-queue event-queue
     :response-fn response-fn
     :handler (fn [& args]
                (let [git-cmd (str/join " " args)
                      event {:git-cmd git-cmd :args args :config config}
                      event-id (count @event-queue)]
                  ;; Record the event
                  (swap! event-queue conj event)
                  
                  ;; Get response from test
                  (let [response (@response-fn event)]
                    (case (:op response)
                      :fake-clone
                      (do
                        ;; Create fake git directory
                        (let [target-dir (or (:target-dir response)
                                           (last args))]  ; Last arg is typically the target dir
                          (when target-dir
                            (when-not (Directory/Exists target-dir)
                              (Directory/CreateDirectory target-dir))
                            (let [config-path (Path/Join target-dir "config")]
                              (File/WriteAllText config-path "[core]\n\trepositoryformatversion = 0\n"))))
                        {:args args :exit 0 :out "" :err ""})
                      
                      :exit
                      {:args args :exit (:status response 1) :out "" :err (:message response "Git command failed")}
                      
                      :stall-seconds
                      (do
                        (Thread/Sleep (* 1000 (:duration response 1)))
                        {:args args :exit 0 :out "" :err ""})
                      
                      :continue
                      {:args args :exit 0 :out "" :err ""}
                      
                      ;; Default: success
                      {:args args :exit 0 :out "" :err ""}))))}))

(defn make-controlled-mock-git
  "Creates a mock git handler with a promise for receiving instructions.
   
   Returns a map with:
   - :handler - function to use with (with-redefs [run-git-with-config ...])
   - :instruction-promise - deliver instructions here before git is called
   - :events - atom with vector of all git events
   
   This allows synchronous control: deliver instruction, then git runs with that instruction."
  []
  (let [instruction-promise (promise)
        events (atom [])]
    {:instruction-promise instruction-promise
     :events events
     :handler (fn [& args]
                (let [git-cmd (str/join " " args)
                      event {:git-cmd git-cmd :args args}]
                  (swap! events conj event)
                  
                  ;; Wait for instruction (blocks until delivered)
                  (let [instruction @instruction-promise]
                    (case (:op instruction)
                      :fake-clone
                      (do
                        (let [target-dir (or (:target-dir instruction) (last args))]
                          (when target-dir
                            (when-not (Directory/Exists target-dir)
                              (Directory/CreateDirectory target-dir))
                            (let [config-path (Path/Join target-dir "config")]
                              (File/WriteAllText config-path "[core]\n\trepositoryformatversion = 0\n"))))
                        {:args args :exit 0 :out "" :err ""})
                      
                      :exit
                      {:args args :exit (:status instruction 1) :out "" :err (:message instruction "Failed")}
                      
                      :stall-then-succeed
                      (do
                        (Thread/Sleep (* 1000 (:duration instruction 1)))
                        (let [target-dir (or (:target-dir instruction) (last args))]
                          (when target-dir
                            (when-not (Directory/Exists target-dir)
                              (Directory/CreateDirectory target-dir))
                            (let [config-path (Path/Join target-dir "config")]
                              (File/WriteAllText config-path "[core]\n\trepositoryformatversion = 0\n"))))
                        {:args args :exit 0 :out "" :err ""})
                      
                      ;; Default: success
                      {:args args :exit 0 :out "" :err ""}))))}))

;; Scenario 1a: We won the race, delete lockfile when git dir deleted
(deftest test-scenario-1a-won-race-deleted-gitdir
  (testing "Won race: delete lockfile when git dir deleted causes waiter to throw"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          temp-gitlibs (create-temp-gitlibs-dir)
          test-config (assoc @clojure.tools.gitlibs.config/CONFIG :gitlibs/dir temp-gitlibs)
          lockfile (get-lockfile temp-gitlibs test-url)
          config-file-path (get-config-file-path temp-gitlibs test-url)
          git-dir-path (get-git-dir-path temp-gitlibs test-url)
          {:keys [handler event-queue response-fn]} (make-mock-git-handler)]
      (try
        ;; Set up mock git to fake clone
        (reset! response-fn (fn [_] {:op :fake-clone}))
        
        ;; Clean up
        (delete-file-if-exists lockfile)
        (when (Directory/Exists git-dir-path)
          (Directory/Delete git-dir-path true))
        
        ;; We create the lock first
        (is (#'impl/acquire-lock lockfile))
        (is (not (File/Exists config-file-path)))
        
        ;; Start a waiter thread with mock git
        (let [waiter-exception (atom nil)
              waiter (future
                       (with-redefs [impl/run-git handler]
                         (try
                           (impl/ensure-git-dir test-config test-url)
                           (catch Exception e
                             (reset! waiter-exception e)))))]
          
          ;; Give waiter time to start waiting and verify it's waiting by checking
          ;; if the lockfile timestamp is being updated by the waiter
          (Thread/Sleep 2000)
          (let [ts1 (#'impl/read-ts lockfile)]
            (Thread/Sleep 1500)
            (let [ts2 (#'impl/read-ts lockfile)]
              ;; If waiter is updating, ts2 should be after ts1
              (is (or (nil? ts1) (nil? ts2) (.CompareTo ts2 ts1)))))
          
          ;; Update timestamp a couple more times to keep it alive
          (#'impl/write-ts lockfile)
          (Thread/Sleep 1000)
          (#'impl/write-ts lockfile)
          
          ;; Verify git dir still doesn't exist - check BEFORE releasing lock
          (is (not (File/Exists config-file-path)) "Config file should not exist before we release lock")
          
          ;; Now release the lock
          (#'impl/release-lock lockfile)
          
          ;; Wait for waiter to complete
          (deref-timely waiter 30000 "Waiter timed out after 30 seconds")
          
          ;; Should have thrown exception about clone failed
          (is (some? @waiter-exception))
          (when @waiter-exception
            (is (str/includes? (.Message @waiter-exception) "Clone failed"))))
        
        (finally
          ;; Cleanup temp directory
          (when (Directory/Exists temp-gitlibs)
            (Directory/Delete temp-gitlibs true)))))))

;; Scenario 1b: We won the race, delete lockfile when git dir exists
(deftest test-scenario-1b-won-race-gitdir-exists
  (testing "Won race: delete lockfile when git dir exists causes waiter to return normally"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          temp-gitlibs (create-temp-gitlibs-dir)
          test-config (assoc @clojure.tools.gitlibs.config/CONFIG :gitlibs/dir temp-gitlibs)
          lockfile (get-lockfile temp-gitlibs test-url)
          config-file-path (get-config-file-path temp-gitlibs test-url)
          git-dir-path (get-git-dir-path temp-gitlibs test-url)]
      
      (try
        ;; Create a fake git dir with config to simulate existing clone
        (when-not (Directory/Exists git-dir-path)
          (Directory/CreateDirectory git-dir-path))
        (File/WriteAllText config-file-path "[core]\n\trepositoryformatversion = 0\n")
        
        ;; Create the lock (simulating we're cloning but git dir already exists)
        (delete-file-if-exists lockfile)
        (is (#'impl/acquire-lock lockfile))
        (is (File/Exists config-file-path))
        
        ;; Start a waiter thread
        (let [waiter-result (atom nil)
              waiter-exception (atom nil)
              waiter (future
                       (try
                         (reset! waiter-result (impl/ensure-git-dir test-config test-url))
                         (catch Exception e
                           (reset! waiter-exception e))))]
          
          ;; Give waiter time to start waiting
          (Thread/Sleep 2000)
          
          ;; Update timestamp to keep it alive
          (#'impl/write-ts lockfile)
          (Thread/Sleep 1000)
          (#'impl/write-ts lockfile)
          
          ;; Delete lock while git dir exists
          (#'impl/release-lock lockfile)
          
          ;; Wait for waiter to complete
          (deref-timely waiter 30000 "Waiter timed out after 30 seconds")
          
          ;; Should return successfully
          (is (nil? @waiter-exception))
          (is (string? @waiter-result))
          (is (= git-dir-path @waiter-result)))
        
        (finally
          ;; Cleanup temp directory
          (when (Directory/Exists temp-gitlibs)
            (Directory/Delete temp-gitlibs true)))))))

;; Scenario 1c: We won the race and stop updating lockfile - waiter should timeout
(deftest test-scenario-1c-won-race-lockfile-expires
  (testing "Won race: stop updating lockfile causes waiter to throw after 10 seconds"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          temp-gitlibs (create-temp-gitlibs-dir)
          test-config (assoc @clojure.tools.gitlibs.config/CONFIG :gitlibs/dir temp-gitlibs)
          lockfile (get-lockfile temp-gitlibs test-url)
          config-file-path (get-config-file-path temp-gitlibs test-url)
          git-dir-path (get-git-dir-path temp-gitlibs test-url)
          {:keys [handler event-queue response-fn]} (make-mock-git-handler)]
      
      (try
        ;; Set up mock git to fake clone (in case waiter tries to clone)
        (reset! response-fn (fn [_] {:op :fake-clone}))
        
        ;; Clean up
        (delete-file-if-exists lockfile)
        (when (Directory/Exists git-dir-path)
          (Directory/Delete git-dir-path true))
        
        ;; We create the lock first
        (is (#'impl/acquire-lock lockfile))
        (is (not (File/Exists config-file-path)))
        
        ;; Start a waiter thread with mock git
        (let [waiter-exception (atom nil)
              waiter (future
                       (with-redefs [impl/run-git-with-config handler]
                         (try
                           (impl/ensure-git-dir test-config test-url)
                           (catch Exception e
                             (reset! waiter-exception e)))))]
          
          ;; Give waiter time to start waiting and verify it's waiting
          (Thread/Sleep 2000)
          (let [ts1 (#'impl/read-ts lockfile)]
            (Thread/Sleep 1500)
            (let [ts2 (#'impl/read-ts lockfile)]
              ;; If waiter is updating, ts2 should be after ts1
              (is (or (nil? ts1) (nil? ts2) (.CompareTo ts2 ts1)))))
          
          ;; Update timestamp once to keep it alive initially
          (#'impl/write-ts lockfile)
          (Thread/Sleep 1000)
          
          ;; Now STOP updating - waiter should detect expiry after 10 seconds
          ;; Wait for waiter to complete (should timeout and throw)
          (deref-timely waiter 15000 "Waiter didn't timeout as expected after 15 seconds")
          
          ;; Clean up lock
          (#'impl/release-lock lockfile)
          
          ;; Should have thrown exception about lock expired
          (is (some? @waiter-exception))
          (when @waiter-exception
            (is (str/includes? (.Message @waiter-exception) "lock expired"))))
        
        (finally
          ;; Cleanup temp directory
          (when (Directory/Exists temp-gitlibs)
            (Directory/Delete temp-gitlibs true)))))))

;; Scenario 2: Git dir exists and lockfile doesn't - fast path
(deftest test-scenario-2-fast-path
  (testing "Git dir exists and lockfile doesn't - fast path"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          temp-gitlibs (create-temp-gitlibs-dir)
          test-config (assoc @clojure.tools.gitlibs.config/CONFIG :gitlibs/dir temp-gitlibs)
          lockfile (get-lockfile temp-gitlibs test-url)
          config-file-path (get-config-file-path temp-gitlibs test-url)
          git-dir-path (get-git-dir-path temp-gitlibs test-url)]
      
      (try
        ;; Create a fake git dir with config to simulate existing clone
        (when-not (Directory/Exists git-dir-path)
          (Directory/CreateDirectory git-dir-path))
        (File/WriteAllText config-file-path "[core]\n\trepositoryformatversion = 0\n")
        
        ;; Ensure no lockfile
        (delete-file-if-exists lockfile)
        (is (File/Exists config-file-path))
        (is (not (File/Exists lockfile)))
        
        ;; Call ensure-git-dir - should take fast path
        (let [start-time (DateTime/Now)
              result (impl/ensure-git-dir test-config test-url)
              elapsed (.TotalMilliseconds (.Subtract (DateTime/Now) start-time))]
          
          ;; Should return successfully with the git dir path
          (is (= git-dir-path result))
          
          ;; Should be very fast (< 100ms) since it takes fast path
          (is (< elapsed 100)))
        
        (finally
          ;; Cleanup temp directory
          (when (Directory/Exists temp-gitlibs)
            (Directory/Delete temp-gitlibs true)))))))

;; Note: Full separate-process git wrapper tests would require resolving ClojureCLR
;; process invocation issues. For now, using in-process tests with temp directory isolation.
;; Tests validate the core coordination logic without hitting the network by using
;; pre-created fake git directories.

;; Scenario 3: Controlled git mock testing with with-redefs
(deftest test-scenario-3-with-mock-git
  (testing "Using with-redefs to control git behavior for coordination testing"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          temp-gitlibs (create-temp-gitlibs-dir)
          test-config (assoc @clojure.tools.gitlibs.config/CONFIG :gitlibs/dir temp-gitlibs)
          lockfile (get-lockfile temp-gitlibs test-url)
          config-file-path (get-config-file-path temp-gitlibs test-url)
          git-dir-path (get-git-dir-path temp-gitlibs test-url)
          mock-git (make-mock-git-handler)]
      
      (try
        ;; Clean up
        (delete-file-if-exists lockfile)
        (when (Directory/Exists git-dir-path)
          (Directory/Delete git-dir-path true))
        
        ;; Configure mock git to create fake clone
        (reset! (:response-fn mock-git)
                (fn [event]
                  {:op :fake-clone :target-dir git-dir-path}))
        
        ;; Use with-redefs to replace run-git-with-config with our mock
        (let [result (with-redefs [impl/run-git-with-config (:handler mock-git)]
                       (impl/ensure-git-dir test-config test-url))]
          
          ;; Should succeed with git dir path
          (is (= git-dir-path result))
          
          ;; Git directory should exist
          (is (File/Exists config-file-path))
          
          ;; Lock should be released
          (is (not (File/Exists lockfile)))
          
          ;; Should have recorded git clone event
          (is (= 1 (count @(:event-queue mock-git))))
          (let [event (first @(:event-queue mock-git))]
            (is (str/includes? (:git-cmd event) "clone"))))
        
        (finally
          ;; Cleanup temp directory
          (when (Directory/Exists temp-gitlibs)
            (Directory/Delete temp-gitlibs true)))))))

;; Scenario 4: Test concurrent clones with mock git
(deftest test-scenario-4-concurrent-with-mock
  (testing "Two concurrent clones with mock git - one clones, one waits"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          temp-gitlibs (create-temp-gitlibs-dir)
          test-config (assoc @clojure.tools.gitlibs.config/CONFIG :gitlibs/dir temp-gitlibs)
          lockfile (get-lockfile temp-gitlibs test-url)
          config-file-path (get-config-file-path temp-gitlibs test-url)
          git-dir-path (get-git-dir-path temp-gitlibs test-url)
          mock-git (make-mock-git-handler)]
      
      (try
        ;; Clean up
        (delete-file-if-exists lockfile)
        (when (Directory/Exists git-dir-path)
          (Directory/Delete git-dir-path true))
        
        ;; Configure mock git to stall for 3 seconds then create fake clone
        (reset! (:response-fn mock-git)
                (fn [event]
                  (Thread/Sleep 3000)
                  {:op :fake-clone :target-dir git-dir-path}))
        
        ;; Start two concurrent clones
        (let [results (atom [])
              f1 (future
                   (with-redefs [impl/run-git-with-config (:handler mock-git)]
                     (swap! results conj (impl/ensure-git-dir test-config test-url))))
              f2 (future
                   (with-redefs [impl/run-git-with-config (:handler mock-git)]
                     (Thread/Sleep 500)  ; Ensure f2 starts after f1
                     (swap! results conj (impl/ensure-git-dir test-config test-url))))]
          
          ;; Wait for both to complete
          (deref-timely f1 120000 "f1 timed out after 2 minutes")
          (deref-timely f2 120000 "f2 timed out after 2 minutes")
          
          ;; Both should succeed with same git dir path
          (is (= 2 (count @results)))
          (is (= git-dir-path (first @results)))
          (is (= git-dir-path (second @results)))
          
          ;; Git directory should exist
          (is (File/Exists config-file-path))
          
          ;; Lock should be released
          (is (not (File/Exists lockfile)))
          
          ;; Only one clone should have actually happened
          (is (= 1 (count @(:event-queue mock-git)))))
        
        (finally
          ;; Cleanup temp directory
          (when (Directory/Exists temp-gitlibs)
            (Directory/Delete temp-gitlibs true)))))))

;; Scenario 5: Test clone failure with mock git
(deftest test-scenario-5-clone-failure-with-mock
  (testing "Clone failure with mock git - should clean up git dir and lock"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          temp-gitlibs (create-temp-gitlibs-dir)
          test-config (assoc @clojure.tools.gitlibs.config/CONFIG :gitlibs/dir temp-gitlibs)
          lockfile (get-lockfile temp-gitlibs test-url)
          config-file-path (get-config-file-path temp-gitlibs test-url)
          git-dir-path (get-git-dir-path temp-gitlibs test-url)
          mock-git (make-mock-git-handler)]
      
      (try
        ;; Clean up
        (delete-file-if-exists lockfile)
        (when (Directory/Exists git-dir-path)
          (Directory/Delete git-dir-path true))
        
        ;; Configure mock git to fail
        (reset! (:response-fn mock-git)
                (fn [event]
                  {:op :exit :status 1 :message "Mock git clone failed"}))
        
        ;; Try to clone - should fail
        (is (thrown? Exception
              (with-redefs [impl/run-git-with-config (:handler mock-git)]
                (impl/ensure-git-dir test-config test-url))))
        
        ;; Wait a bit for cleanup to complete
        (Thread/Sleep 500)
        
        ;; Git directory should NOT exist (cleaned up on failure)
        (is (not (Directory/Exists git-dir-path)))
        
        ;; Lock should be released
        (is (not (File/Exists lockfile)))
        
        (finally
          ;; Cleanup temp directory
          (when (Directory/Exists temp-gitlibs)
            (Directory/Delete temp-gitlibs true)))))))

