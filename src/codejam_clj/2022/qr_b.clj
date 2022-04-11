(ns codejam-clj.2022.qr-b
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join]]))

;; 3D Printing (13pts)


(defn calc-ink [{:keys [r over] :as state} idx]
  (let [target (nth r idx)]
    (if (>= target over)
      (reduced (assoc r idx (- target over)))
      {:r (assoc r idx 0)
       :over (- over target)})))


;; (reduce calc-ink {:r r
;;                   :over over} (range 4))

(defn solve [p1 p2 p3]
  (let [[c1 m1 y1 k1] p1
        [c2 m2 y2 k2] p2
        [c3 m3 y3 k3] p3
        c (min c1 c2 c3)
        m (min m1 m2 m3)
        y (min y1 y2 y3)
        k (min k1 k2 k3)
        sum (+ c m y k)
        over (- sum 1000000)]
    (if (> 0 over)
      "IMPOSSIBLE"
      (->> (reduce calc-ink {:r [c m y k]
                             :over over} (range 4))
           (join " ")))))


;; (defn solve [p1 p2 p3]
;;   (prn r c))


(defn get-ink []
  (into [] (map #(Integer/parseInt %)
                (re-seq #"\d+" (read-line)))))

; auxiliary logic.
(defn do-one [i]
  (let [p1 (get-ink)
        p2 (get-ink)
        p3 (get-ink)
        answer (solve p1 p2 p3)]
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
  [r c] (map #(Integer/parseInt %) (re-seq #"\d+" "3 4"))

  (def file (-> "2022/qr-b.txt" io/resource))
  (-main file)


  (def p1 [300000 200000 300000 500000])
  (def p2 [300000 200000 500000 300000])
  (def p3 [300000 500000 300000 200000])


  (def over 350000)



  (solve [1000000 1000000 0 0]
         [0 1000000 1000000 1000000]
         [999999 999999 999999 999999])


  (def r [300000 500000 300000 200000])

  (join " " r)

  (let [[c m y k] r
        sum (+ c m y k)
        over (- 1000000 sum)]
    [sum over])






  (assoc [1 2 3 4] 2 9)

  '())
