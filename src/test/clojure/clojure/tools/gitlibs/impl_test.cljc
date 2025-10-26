(ns clojure.tools.gitlibs.impl-test
  (:require
    [clojure.test :refer :all]
    [clojure.tools.gitlibs.impl :as impl]
    [clojure.clr.io :as cio]
    [clojure.string :as str])
  (:import
    [System DateTime Environment]
    [System.IO Directory DirectoryInfo File Path]
    [System.Threading Thread ThreadStart]))

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

(defn- get-lockfile
  "Returns the lockfile path for a given URL."
  [url]
  (let [git-dir-file (impl/git-dir url)]
    (#'impl/lockfile-for-git-dir git-dir-file)))

(defn- get-config-file-path
  "Returns the config file path for a given URL."
  [url]
  (let [git-dir-file (impl/git-dir url)
        config-file (cio/file-info git-dir-file "config")]
    (.FullName config-file)))

;; Scenario 1a: We won the race and delete lockfile when git dir is deleted
(deftest test-scenario-1a-won-race-deleted-gitdir
  (testing "Won race: delete lockfile when git dir deleted causes waiter to throw"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          git-dir-file (impl/git-dir test-url)
          lockfile (get-lockfile test-url)
          config-file-path (get-config-file-path test-url)]
      
      ;; Clean up
      (delete-dir-recursive git-dir-file)
      (delete-file-if-exists lockfile)
      
      ;; We create the lock first
      (is (#'impl/acquire-lock lockfile))
      (is (not (File/Exists config-file-path)))
      
      ;; Start a waiter thread
      (let [waiter-result (atom nil)
            waiter (future
                     (try
                       (reset! waiter-result (impl/ensure-git-dir test-url))
                       (catch Exception e
                         (reset! waiter-result {:error (.Message e)}))))]
        
        ;; Give waiter time to start waiting
        (Thread/Sleep 2000)
        
        ;; Update timestamp to keep it alive
        (#'impl/write-ts lockfile)
        (Thread/Sleep 1000)
        (#'impl/write-ts lockfile)
        
        ;; Verify git dir still doesn't exist and delete lock
        (is (not (File/Exists config-file-path)))
        (#'impl/release-lock lockfile)
        
        ;; Wait for waiter to complete
        (deref-timely waiter 30000 "Waiter timed out after 30 seconds")
        
        ;; Should have error about clone failed
        (is (map? @waiter-result))
        (is (contains? @waiter-result :error))
        (is (str/includes? (:error @waiter-result) "Clone failed"))))))

;; Scenario 1b: We won the race and delete lockfile when git dir exists
(deftest test-scenario-1b-won-race-gitdir-exists
  (testing "Won race: delete lockfile when git dir exists causes waiter to return normally"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          git-dir-file (impl/git-dir test-url)
          lockfile (get-lockfile test-url)
          config-file-path (get-config-file-path test-url)]
      
      ;; Ensure git dir exists
      (when-not (File/Exists config-file-path)
        (delete-dir-recursive git-dir-file)
        (delete-file-if-exists lockfile)
        (impl/ensure-git-dir test-url))
      
      ;; Create the lock
      (delete-file-if-exists lockfile)
      (is (#'impl/acquire-lock lockfile))
      (is (File/Exists config-file-path))
      
      ;; Start a waiter thread
      (let [waiter-result (atom nil)
            waiter (future
                     (try
                       (reset! waiter-result (impl/ensure-git-dir test-url))
                       (catch Exception e
                         (reset! waiter-result {:error (.Message e)}))))]
        
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
        (is (string? @waiter-result))
        (is (= (.FullName git-dir-file) @waiter-result))))))

;; Scenario 1c: We won the race and stop updating lockfile - waiter should timeout
(deftest test-scenario-1c-won-race-lockfile-expires
  (testing "Won race: stop updating lockfile causes waiter to throw after 10 seconds"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          git-dir-file (impl/git-dir test-url)
          lockfile (get-lockfile test-url)
          config-file-path (get-config-file-path test-url)]
      
      ;; Clean up
      (delete-dir-recursive git-dir-file)
      (delete-file-if-exists lockfile)
      
      ;; We create the lock first
      (is (#'impl/acquire-lock lockfile))
      (is (not (File/Exists config-file-path)))
      
      ;; Start a waiter thread
      (let [waiter-result (atom nil)
            waiter (future
                     (try
                       (reset! waiter-result (impl/ensure-git-dir test-url))
                       (catch Exception e
                         (reset! waiter-result {:error (.Message e)}))))]
        
        ;; Give waiter time to start waiting
        (Thread/Sleep 2000)
        
        ;; Update timestamp once to keep it alive initially
        (#'impl/write-ts lockfile)
        (Thread/Sleep 1000)
        
        ;; Now STOP updating - waiter should detect expiry after 10 seconds
        ;; Don't update anymore, just wait for the timeout
        
        ;; Wait for waiter to complete (should timeout and throw)
        (deref-timely waiter 15000 "Waiter didn't timeout as expected after 15 seconds")
        
        ;; Clean up lock
        (#'impl/release-lock lockfile)
        
        ;; Should have error about lock expired
        (is (map? @waiter-result))
        (is (contains? @waiter-result :error))
        (is (str/includes? (:error @waiter-result) "lock expired"))))))

;; Scenario 2: Git dir exists and lockfile doesn't - fast path
(deftest test-scenario-2-fast-path
  (testing "Git dir exists and lockfile doesn't - fast path"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          git-dir-file (impl/git-dir test-url)
          lockfile (get-lockfile test-url)
          config-file-path (get-config-file-path test-url)]
      
      ;; Ensure git dir exists
      (when-not (File/Exists config-file-path)
        (delete-dir-recursive git-dir-file)
        (delete-file-if-exists lockfile)
        (impl/ensure-git-dir test-url))
      
      ;; Ensure no lockfile
      (delete-file-if-exists lockfile)
      (is (File/Exists config-file-path))
      (is (not (File/Exists lockfile)))
      
      ;; Call ensure-git-dir - should take fast path
      (let [start-time (DateTime/Now)
            result (impl/ensure-git-dir test-url)
            elapsed (.TotalMilliseconds (.Subtract (DateTime/Now) start-time))]
        
        ;; Should return successfully with the git dir path
        (is (= (.FullName git-dir-file) result))
        
        ;; Should be very fast (< 100ms) since it takes fast path
        (is (< elapsed 100))))))

;; Scenario 3: Lost race - waiter coordinates with cloner
(deftest test-scenario-3-lost-race-coordination
  (testing "Lost race: waiter coordinates with cloner thread"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          git-dir-file (impl/git-dir test-url)
          lockfile (get-lockfile test-url)
          config-file-path (get-config-file-path test-url)]
      
      ;; Clean up - simulate starting fresh
      (delete-dir-recursive git-dir-file)
      (delete-file-if-exists lockfile)
      
      ;; Start the cloner thread
      (let [cloner-result (atom nil)
            cloner (future
                     (try
                       (reset! cloner-result (impl/ensure-git-dir test-url))
                       (catch Exception e
                         (reset! cloner-result {:error (.Message e)}))))]
        
        ;; Give cloner time to acquire lock and start cloning
        (Thread/Sleep 1000)
        
        ;; Verify lock exists
        (is (File/Exists lockfile))
        
        ;; Start waiter thread - should wait for cloner
        (let [waiter-result (atom nil)
              waiter (future
                       (try
                         (reset! waiter-result (impl/ensure-git-dir test-url))
                         (catch Exception e
                           (reset! waiter-result {:error (.Message e)}))))]
          
          ;; Wait for both to complete
          (deref-timely cloner 120000 "Cloner timed out after 2 minutes")
          (deref-timely waiter 120000 "Waiter timed out after 2 minutes")
          
          ;; Both should succeed
          (is (string? @cloner-result))
          (is (= (.FullName git-dir-file) @cloner-result))
          (is (string? @waiter-result))
          (is (= (.FullName git-dir-file) @waiter-result))
          (is (File/Exists config-file-path))
          (is (not (File/Exists lockfile)))))))
)
