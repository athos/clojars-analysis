(ns clojars-analysis.stats
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io PushbackReader]))

(def lines
  (with-open [r (io/reader "data/verified_groups.txt")]
    (doall (line-seq r))))

(def verified-groups (set lines))

(defn- write-chart [out title data]
  (binding [*out* (if out (io/writer out) *out*)]
    (-> {:data {:values data}
         :repeat {:layer ["unverified" "verified"]}
         :spec
         {:mark "bar"
          :encoding
          {:x {:field "month" :type "ordinal"}
           :y {:field {:repeat "layer"}
               :type "quantitative"
               :title title}
           :color {:datum {:repeat "layer"} :type "nominal"}
           :xOffset {:datum {:repeat "layer"}}}}}
        (json/generate-stream *out* {:pretty true}))))

(defn artifacts [{:keys [out]}]
  (->> (for [m (range 4 11)
             :let [file (format "data/2021%02d.edn" m)
                   artifacts (with-open [r (PushbackReader. (io/reader file))]
                               (edn/read r))
                   all (->> artifacts
                            (map (comp count val))
                            (apply +))
                   verified (->> (for [[k v] artifacts
                                       :let [group (namespace (symbol k))]
                                       :when (verified-groups group)]
                                   (count v))
                                 (apply +))]]
         {:month m
          :verified verified
          :unverified (- all verified)})
       (write-chart out "Number of Releases")))

(defn groups [{:keys [out]}]
  (->> (for [m (range 4 11)
             :let [file (format "data/2021%02d_groups.txt" m)
                   groups (with-open [r (io/reader file)]
                            (doall (line-seq r)))
                   verified (filter verified-groups groups)]]
         {:month m
          :verified (count verified)
          :unverified (- (count groups) (count verified))})
       (write-chart out "Number of Groups")))

(defn- build-tree [lines]
  (letfn [(walk [path name t]
            (if (empty? t)
              {:name name :parent path :count 1}
              (let [path' (conj path name)
                    children (for [[name t] t]
                               (walk path' name t))
                    total (->> children
                               (map :count)
                               (apply +))
                    children' (remove #(= (:count %) 1) children)
                    total' (->> children'
                                (map :count)
                                (apply +))]
                {:name name, :parent path, :count total,
                 :children (->> (cond->> children'
                                  (and (not= total total')
                                       (seq children'))
                                  (concat [{:name "*"
                                            :parent path'
                                            :count (- total total')}]))
                                (sort-by :name))})))]
    (let [children (->> lines
                        (reduce #(assoc-in %1 (str/split %2 #"\.") {}) {})
                        (reduce (fn [acc [name t]]
                                  (conj acc (walk [] name t)))
                                [])
                        (sort-by :name))]
      {:name "<root>", :parent [], :children children,
       :count (apply + (map :count children))})))

(defn tree-map [{:keys [out]}]
  (let [tree (build-tree lines)]
    (letfn [(step [{:keys [name parent count children]}]
              (printf "%s,%s,%d\n"
                      (str/join \. (conj parent name))
                      (if (or (seq parent) (= name "<root>"))
                        (str/join \. parent)
                        "<root>")
                      count)
              (run! step children))]
      (binding [*out* (if out (io/writer out) *out*)]
        (step tree)))))
