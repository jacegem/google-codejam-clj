(ns codejam-clj.2022.qr-d
  (:require [clojure.java.io :as io]))


(defn trigger [{:keys [data sum points] :as state} id]
  (let [module (get data id)
        [module points] (if (:active module)
                          [(assoc module :active false)
                           (conj points (:point module))]
                          [module points])
        data (assoc data id module)
        parent (:parent module)]

    (if (= parent :abyss)
      {:data data
       :sum (+ sum (apply max points))
       :points []}
      (recur {:data data :sum sum :points points} parent))))


#_(true? (:active {:active true}))

(defn trigger-all [data order]
  (-> (reduce trigger {:data data :sum 0 :points []} order)
      :sum))

#_(trigger {:data data :sum 0 :points []} 4)

#_(def order [4 3])
#_(trigger-all data order)


(defn perms [v]
  (cond (= 1 (count v)) v                        ; one permutation is it's self 
        (= 2 (count v)) [[(second v) (first v)]  ; two items is [[ab][b a]]
                         [(first v) (second v)]]
        :default
        (apply concat
               (for [i (range (count v))]        ; take the first item     
                 (->> (assoc v i (v 0))          ; add it in each position 
                      (#(subvec % 1))            ; find the permutations of 
                      perms                      ; the rest of each of them 
                      (mapv #(conj % (nth v i)))))))) ; then stick the 
                                                 ; one that was assoced back
                                               ; onto the start of each of them   
(defn get-orders [data]
  (->> (filter (fn [[k v]] (= true (:leaf v))) data)
       keys
       (into [])
       perms))

(defn find-parent [{:keys [data] :as state} id]
  (let [leaf? (nil? (some (fn [[k v]] (= id (:parent v))) data))
        data (assoc-in data [id :leaf] leaf?)]
    {:data data}))

(defn get-data [n fs ps]
  (let [data (->> (for [idx (range n)]
                    [(inc idx) {:point (get fs idx)
                                :parent (let [p (get ps idx)]
                                          (if (= p 0)
                                            :abyss
                                            p))
                                :active true}])
                  (into {}))]
    (-> (reduce find-parent {:data data} (range 1 (inc n)))
        :data)))

#_(get-data 5 [3 2 1 4 5] [0 1 1 1 0])

#_(get-data 8 [100 100 100 90 80 100 90 100] [0 1 2 1 2 3 1 3])
#_(get-orders (get-data [60 20 40 50] [0 1 1 2]))


(defn solve [n fs ps]
  #_(prn fs ps)
  (let [data (get-data n fs ps)
        orders (get-orders data)]
    (->> orders
         (map #(trigger-all data %))
         (apply  max))))


; auxiliary logic.
(defn do-one [i]
  (let [n (read-string (read-line))
        fs (->> (map #(Integer/parseInt %)
                     (re-seq #"\d+" (read-line)))
                (into []))
        ps (->> (map #(Integer/parseInt %)
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




  "3 2 1 4 5
   0 1 1 1 0"

  (def data
    {1 {:point 3 :parent :abyss :active true :leaf false}
     2 {:point 2 :parent 1 :active true :leaf true}
     3 {:point 1 :parent 1 :active true :leaf true}
     4 {:point 4 :parent 1 :active true :leaf true}
     5 {:point 5 :parent :abyss :active true :leaf true}})

  "60 20 40 50
  0 1 1 2"
  (def data
    {1 {:point 60 :parent :abyss :active true :leaf false}
     2 {:point 20 :parent 1 :active true :leaf false}
     3 {:point 40 :parent 1 :active true :leaf true}
     4 {:point 50 :parent 2 :active true :leaf true}})

  (defn perms [v]
    (cond (= 1 (count v)) v                        ; one permutation is it's self 
          (= 2 (count v)) [[(second v) (first v)]  ; two items is [[ab][b a]]
                           [(first v) (second v)]]
          :default
          (apply concat
                 (for [i (range (count v))]        ; take the first item     
                   (->> (assoc v i (v 0))          ; add it in each position 
                        (#(subvec % 1))            ; find the permutations of 
                        perms                      ; the rest of each of them 
                        (mapv #(conj % (nth v i)))))))) ; then stick the 
                                                 ; one that was assoced back
                                                 ; onto the start of each of them   

  (perms [1 2 3])
  (->>
   (filter (fn [[k v]] (= true (:leaf v))) data)
   keys
   (into [])
   perms)

  (def order [4 3 2 1])


  (defn trigger [{:keys [data sum points] :as state} idx]
    (let [module (get data idx)
          [module points] (if (= true (:active module))
                            [(assoc module :active false)
                             (conj points (:point module))]
                            [module points])
          data (assoc data idx module)
          parent (:parent module)]

      (if (= parent :abyss)
        {:data data
         :sum (+ sum (apply max points))
         :points []}
        (recur {:data data :sum sum :points points} parent))))

  (trigger {:data data :sum 0 :points []} 3)
  (trigger {:data
            {1 {:point 60, :parent :abyss, :active false, :leaf false},
             2 {:point 20, :parent 1, :active true, :leaf false},
             3 {:point 40, :parent 1, :active false, :leaf true},
             4 {:point 50, :parent 2, :active true, :leaf true}},
            :sum 60,
            :points []} 4)

  (defn trigger-all [data order]
    (-> (reduce trigger {:data data :sum 0 :points []} order)
        :sum))

  (trigger-all data [4 3])

  (def grids
    (->> (filter (fn [[k v]] (= true (:leaf v))) data)
         keys
         (into [])
         perms))

  (apply max
         (map #(trigger-all data %) grids))


  (def data
    (->> (let [fs [60 20 40 50]
               ps [0 1 1 2]]
           (for [idx (range 4)]
             [(inc idx) {:point (get fs idx)
                         :parent (let [p (get ps idx)]
                                   (if (= p 0)
                                     :abyss
                                     p))
                         :acitve true}]))
         (into {})))

  data
  (nil? (some (fn [[k v]] (= 1 (:parent v))) data))


  (defn find-parent [{:keys [data] :as state} id]
    (let [leaf? (nil? (some (fn [[k v]] (= id (:parent v))) data))
          data (assoc-in data [id :leaf] leaf?)]
      {:data data}))

  (reduce find-parent {:data data} (range 1 (inc 4)))













  '())