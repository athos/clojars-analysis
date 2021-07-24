(ns clojars-analysis.dump
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pp]
            [clojure.string :as str])
  (:import [java.io PushbackReader]))

(def ^:dynamic *cwd* ".")

(defn exec [& cmd-strs]
  (let [cmd-str (str/join \space cmd-strs)
        {:keys [out err]} (->> [:dir *cwd*]
                               (concat (str/split cmd-str #" +"))
                               (apply shell/sh))]
    (if (seq err)
      (throw (ex-info err {}))
      out)))

(defn current-commit-sha []
  (str/trim-newline (exec "git rev-parse HEAD")))

(defn load-edn [edn-file]
  (with-open [r (io/reader edn-file)]
    (->> (line-seq r)
         (map edn/read-string)
         (reduce (fn [ret item]
                   (let [name (str (:group_name item) \/ (:jar_name item))]
                     (update ret name (fnil conj #{}) (:version item))))
                 {}))))

(defn load-logs [repo-path start-date end-date]
  (binding [*cwd* repo-path]
    (let [head (current-commit-sha)
          commits (->> (exec "git rev-list"
                             "--after" start-date
                             "--before" end-date
                             "HEAD")
                       str/split-lines)]
      (try
        (reduce (fn [ret sha]
                  (exec "git reset --hard" sha)
                  (let [edn-file (io/file repo-path "data" "stable.edn")
                        m (load-edn edn-file)]
                    (merge-with into ret m)))
                {}
                commits)
        (catch Throwable t
          (binding [*out* *err*]
            (println (ex-message t))))
        (finally
          (exec "git reset --hard" head))))))

(defn dump-libs [{:keys [repo start end out] :or {out "out.edn"}}]
  (with-open [w (io/writer out)]
    (binding [*out* w
              *print-length* nil
              *print-level* nil]
      (pp/pprint (load-logs repo start end)))))

(defn dump-groups [{:keys [in out] :or {out "groups.txt"}}]
  (with-open [r (PushbackReader. (io/reader in))
              w (io/writer out)]
    (binding [*out* w]
      (let [libs (edn/read r)
            groups (into (sorted-set)
                         (map #(str/replace % #"/[^/]+$" ""))
                         (keys libs))]
        (run! println groups)))))
