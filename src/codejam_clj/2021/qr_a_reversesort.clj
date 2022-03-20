(ns codejam-clj.2021.qr-a-reversesort
  (:require [clojure.java.io :as io]))



(defn solve-reducer [{:keys [n cost elements] :as state} target-idx]
  (let [target-value (nth elements target-idx)
        sub-element (subvec elements (inc target-idx) n)
        min-value (apply min sub-element)
        min-index (.indexOf elements min-value)
        sub-target (subvec elements target-idx (inc min-index))
        sub-reserve (reverse sub-target)
        head (subvec elements 0 target-idx)
        tail (subvec elements (inc min-index) n)

        updated-cost (if (< min-value target-value)
                       (+ cost 1 (- min-index target-idx))
                       (+ cost 1))
        updated-elements (if (< min-value target-value)
                           (into [] (flatten [head sub-reserve tail]))
                           elements)]
    (-> state
        (assoc :cost updated-cost)
        (assoc :elements updated-elements))))

(defn solve [N elements]
  (->
   (reduce solve-reducer {:n N :elements elements :cost 0} (range 0 (dec N)))
   :cost))



; auxiliary logic.
(defn do-one
  [i]
  (let [N (read-string (read-line))
        elements (into [] (map #(Integer/parseInt %) (re-seq #"\d+" (read-line))))
        answer (solve N elements)]
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
  (def file (-> "2021/qr-a.txt" io/resource))
  (-main file)
  '())
