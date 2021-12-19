(ns clojars-analysis.annotate
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [net.cgrand.enlive-html :as html])
  (:import [java.net URL]))

(defn- apparent-domain-name? [group]
  (nat-int? (str/index-of group \.)))

(defn- known-verified-group? [group]
  (boolean (re-find #"^(?:org|net)\.clojars\." group)))

(defn- group-member [group]
  (let [{:keys [body]} (http/get (str "https://clojars.org/api/groups/" group)
                                 {:as :json})]
    (:user (first body))))

(def ^:dynamic *wait-interval* 1000)

(defn- wait []
  (Thread/sleep *wait-interval*))

(defn- check-verification [group]
  (let [user (group-member group)
        _ (wait)
        verified-groups (-> (URL. (str "https://clojars.org/users/" user))
                            html/html-resource
                            (html/select [:div
                                          (html/has [:> :span.verified-group])
                                          html/first-child
                                          html/first-child])
                            (#(into #{} (map (comp first :content)) %)))]
    (wait)
    (contains? verified-groups group)))

(defn- verified? [group]
  (and (apparent-domain-name? group)
       (or (known-verified-group? group)
           (check-verification group))))

(defn- annotate-group [group]
  {:group group :verified (verified? group)})

(defn annotate [{:keys [in out wait-interval] :or {wait-interval 1000}}]
  (with-open [r (io/reader in)
              w (io/writer out)]
    (binding [*out* w, *wait-interval* wait-interval]
      (doseq [group (line-seq r)]
        (prn (annotate-group group))))))
