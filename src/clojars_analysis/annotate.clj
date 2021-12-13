(ns clojars-analysis.annotate
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [net.cgrand.enlive-html :as html])
  (:import [java.net URL]))

(defn- apparent-domain-name? [group]
  (nat-int? (str/index-of group \.)))

(defn- known-verified-group? [group]
  (boolean (re-find #"^(?:org|net)\.clojars\." group)))

(defn- check-verification [group]
  (-> (URL. (str "https://clojars.org/groups/" group))
      html/html-resource
      (html/select [:span.verified-group])
      not-empty
      boolean))

(defn- verified? [group]
  (and (apparent-domain-name? group)
       (or (known-verified-group? group)
           (check-verification group))))

(defn- annotate-group [group]
  {:group group :verified (verified? group)})

(defn annotate [{:keys [in out]}]
  (with-open [r (io/reader in)
              w (io/writer out)]
    (binding [*out* w]
      (doseq [group (line-seq r)]
        (prn (annotate-group group))
        (Thread/sleep 1000)))))
