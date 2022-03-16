(ns codejam-clj.2021.qr-a
  (:require [clojure.java.io :as io]))
            ;; [clojure.string :as str]))

;; ; core logic
;; (defn solve
;;   [num]
;;   (let [int-num (Integer/parseInt num)
;;         new-num (Integer/parseInt (str/replace num #"4" "3"))]
;;     [new-num (- int-num new-num)]))

(defn solve-2 [s]
  (let [nums (map (fn [n]
                    (if (= n 4)
                      [2 2]
                      [n 0])) (map #(-> % str Integer/parseInt) s))
        first-num (apply str (map first nums))
        second-num (apply str (map second nums))]
    [first-num second-num]))

;; (solve-2 "123450000000123123123123123123123")
;; (Integer/parseInt "123450000000123123123123123123123")
;; (apply str  [0 0 1 2])
;; (drop-while #(= 0 %) [0 0 1 2])

; auxiliary logic.
(defn do-one
  [i]
  (let [N (read-line)
        [A B] (solve-2 N)]
    (println (format "Case #%d: %s %s" i A B))))


(defn do-stdin []
  (let [T (read-string (read-line))]
    (dotimes [i T]
      (do-one (inc i)))))

(defn do-file [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (binding [*in* rdr]
      (do-stdin))))

(defn -main [& [fname]]
  (if fname
    (do-file fname)
    (do-stdin)))

(defn filename [year round problem]
  (format "resources/%d/%s-%s.txt" year round problem))

(comment
  (solve-2 "3")
  (filename 2019 "qr" "a")
  (do-file (filename 2019 "qr" "a"))
  '())

(-main)




