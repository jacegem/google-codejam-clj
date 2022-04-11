(ns codejam-clj.2022.qr_a
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join]]))

;; Punched Cards (11pts)

(defn row-1 [c]
  (let [head ".."
        body (->> (repeat "+")
                  (take c)
                  (join "-"))]
    (str head body)))
(row-1 4)

(defn row-2 [c]
  (let [head ".."
        body (->> (repeat "|")
                  (take c)
                  (join "."))]
    (str head body)))
(row-2 4)

(defn row-3 [c]
  (->> (repeat "+")
       (take (inc c))
       (join "-")))
(row-3 4)

(defn row-4 [c]
  (->> (repeat "|")
       (take (inc c))
       (join ".")))
(row-4 4)

(defn get-row [r c]
  (cond
    (= r 0) [(row-1 c) (row-2 c) (row-3 c)]
    :else [(row-4 c) (row-3 c)]))

(defn solve [r c]
  (->>
   (mapcat #(get-row % c) (range r))
   (join "\n")))

; auxiliary logic.
(defn do-one
  [i]
  (let [[r c] (into [] (map #(Integer/parseInt %)
                            (re-seq #"\d+" (read-line))))
        answer (solve r c)]
    (println (format "Case #%d:" i))
    (println answer)))

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

  (def file (-> "2022/qr-a.txt" io/resource))
  (-main file)








  (get-row 1 4)
  (join "\n"
        (solve 3 4))


;;   ("+" * 4)



;;   (clojure.string/join "_" ["+" "+"])

;;    (get-row 1 :)

  "
3
3 4
2 2
2 3
   "
  '())
