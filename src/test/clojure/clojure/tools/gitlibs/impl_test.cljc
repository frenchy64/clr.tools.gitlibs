(ns clojure.tools.gitlibs.impl-test
  (:require
    [clojure.test :refer :all]
    [clojure.tools.gitlibs.impl :as impl]
    [clojure.clr.io :as cio])
  (:import
    [System DateTime]
    [System.IO Directory DirectoryInfo File]
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

(deftest test-concurrent-clone-coordination
  (testing "Two racing processes - both should succeed and coordinate via lock"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          git-dir-file (impl/git-dir test-url)
          lockfile (str (.FullName git-dir-file) ".lock")
          results (atom [])]
      
      ;; Clean up any existing clone and lock file
      (delete-dir-recursive git-dir-file)
      (when (File/Exists lockfile)
        (File/Delete lockfile))
      
      ;; Start two concurrent processes
      (let [f1 (future
                 (try
                   (let [result (impl/ensure-git-dir test-url)]
                     (swap! results conj {:process 1 :success true :result result}))
                   (catch Exception e
                     (swap! results conj {:process 1 :success false :error (.Message e)}))))
            f2 (future
                 (try
                   (let [result (impl/ensure-git-dir test-url)]
                     (swap! results conj {:process 2 :success true :result result}))
                   (catch Exception e
                     (swap! results conj {:process 2 :success false :error (.Message e)}))))]
        
        ;; Wait for both to complete
        @f1
        @f2
        
        ;; Both should succeed
        (is (= 2 (count @results)))
        (is (every? :success @results))
        
        ;; Git dir should exist with config file
        (let [config-file (cio/file-info git-dir-file "config")]
          (is (.Exists config-file)))
        
        ;; Lock file should be cleaned up
        (is (not (File/Exists lockfile)))))))

(deftest test-fast-path-after-successful-clone
  (testing "Process running after successful clone takes fast path"
    (let [test-url "https://github.com/clojure/spec.alpha.git"
          git-dir-file (impl/git-dir test-url)
          lockfile (str (.FullName git-dir-file) ".lock")
          config-file (cio/file-info git-dir-file "config")]
      
      ;; Ensure clone exists from previous test or create it
      (when-not (.Exists config-file)
        (delete-dir-recursive git-dir-file)
        (when (File/Exists lockfile)
          (File/Delete lockfile))
        (impl/ensure-git-dir test-url))
      
      ;; Verify config exists and no lock file
      (is (.Exists config-file))
      (is (not (File/Exists lockfile)))
      
      ;; Now call ensure-git-dir again - should take fast path
      (let [start-time (DateTime/Now)
            result (impl/ensure-git-dir test-url)
            elapsed (.TotalMilliseconds (.Subtract (DateTime/Now) start-time))]
        
        ;; Should return successfully
        (is result)
        (is (= (.FullName git-dir-file) result))
        
        ;; Should be very fast (< 100ms) since it takes fast path
        (is (< elapsed 100))
        
        ;; No lock file should have been created
        (is (not (File/Exists lockfile)))))))
