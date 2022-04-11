(ns codejam-clj.2022.qr-c
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join]]))


(defn search [{:keys [sides straight] :as state} idx]
  (let [target (nth sides idx)]
    (if (> target straight)
      (assoc state :straight (inc straight))
      state)))

(defn solve [n sides]
  (-> (reduce search {:sides (sort sides)
                      :straight 0} (range n))
      :straight))

; auxiliary logic.
(defn do-one [i]
  (let [n (read-string (read-line))
        sides (->> (map #(Integer/parseInt %)
                        (re-seq #"\d+" (read-line)))
                   (into []))
        answer (solve n sides)]
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
  (def file (-> "2022/qr-c.txt" io/resource))
  (-main file)

  (solve 4 [6 10 12 8])

  (defn solve [n sides]
    (let [s (sort sides)]
      s))

  (defn search [{:keys [sides straight] :as state} idx]
    (let [target (nth sides idx)]
      (if (> target straight)
        (assoc state :straight (inc straight))
        state)))

  (reduce search {:sides (sort [10 10 7 6 7 4 4 5 7 4])
                  :straight 0} (range 10))


  (def n 100000)
  (def ex
    (->> (take n (repeatedly #(rand-int n)))
         (into [])))
  
    (time (sort ex))

  (time
   (solve n ex))


  (solve 10 [10 10 7 6 7 4 4 5 7 4])

  '())