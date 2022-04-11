(ns codejam-clj.2022.qr-d-2
  (:require [clojure.java.io :as io]))

(defn trigger [{:keys [data sum points] :as state} id]
  (let [module (get data id)
        [module points] (if (:active module)
                          [(assoc module :active false)
                           (conj points (:point module))]
                          [module points])
        data (assoc data id module)
        parent (:parent module)]

    (if (= parent 0)
      {:data data
       :sum (+ sum (apply max points))
       :points []}
      (recur {:data data :sum sum :points points} parent))))


(defn get-data [n fs ps]
  (let [data (->> (for [idx (range n)]
                    [(inc idx) {:point (get fs idx)
                                :parent (get ps idx)
                                :active true}])
                  (into {}))]
    (-> (reduce find-parent {:data data} (range 1 (inc n)))
        :data)))

(defn find-parent [{:keys [data] :as state} id]
  (let [leaf? (nil? (some (fn [[k v]] (= id (:parent v))) data))
        data (assoc-in data [id :leaf] leaf?)]
    {:data data}))

(defn get-order [data]
  (->> (filter (fn [[k v]] (:leaf v)) data)
       (sort-by (fn [[k v]] ((juxt :parent :point) v)))
       keys
       (into [])))

(defn solve [n fs ps]
  (let [data (get-data n fs ps)
        order (get-order data)]
    #_(clojure.pprint/pprint data)
    (-> (reduce trigger {:data data :sum 0 :points []} order)
        :sum)))

; auxiliary logic.
(defn do-one [i]
  (let [n (read-string (read-line))
        fs (->> (map #(Long/parseLong %)
                     (re-seq #"\d+" (read-line)))
                (into []))
        ps (->> (map #(Long/parseLong %)
                     (re-seq #"\d+" (read-line)))
                (into []))
        answer (solve n fs ps)]
    (println (format "Case #%d: %s" i answer))))

(defn do-stdin []
  (let [T (read-string (read-line))]
    (dotimes [i T]
      (do-one (inc i)))))

(defn do-file [fname]
  (with-open [rdr (io/reader fname)]
    (binding [*in* rdr]
      (do-stdin))))

(defn -main [& [fname]]
  (if fname
    (do-file fname)
    (do-stdin)))


(-main)


(comment
  (def file (-> "2022/qr-d.txt" io/resource))
  (-main file)





  (def data
    {1 {:point 3 :parent 0 :active true :leaf false}
     2 {:point 2 :parent 1 :active true :leaf true}
     3 {:point 1 :parent 1 :active true :leaf true}
     4 {:point 4 :parent 1 :active true :leaf true}
     5 {:point 5 :parent 0 :active true :leaf true}})

  "point가 작은 leaf 순서"

  (->>
   (filter (fn [[k v]] (true? (:leaf v))) data)
   (sort-by (fn [[k v]] ((juxt :parent :point) v)))
   keys
   (into []))




  '())