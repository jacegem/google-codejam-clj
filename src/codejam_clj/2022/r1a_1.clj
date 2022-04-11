(ns codejam-clj.2022.r1a-1
  (:require [clojure.java.io :as io]))


(defn reducer [{:keys [data answer] :as state} idx]
  (if (= (inc idx) (count data))
    (apply str (conj answer (get data idx)))
    (let [curr (get data idx)
          next (get data (inc idx))
          answer (conj answer curr)]
      (if (<= (int curr) (int next))
        (-> state
            (assoc :answer (conj answer curr)))
        (-> state
            (assoc :answer answer))))))


(defn solve [s]
  (let [data (->> (seq s)
                  (into []))
        all-same? (every? (fn [v] (= v (get data 0))) data)]
    (if all-same?
      s
      (reduce reducer {:data data :answer []} (range (count data))))))


; auxiliary logic.
(defn do-one [i]
  (let [s (read-line)
        answer (solve s)]
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
  (def file (-> "2022/r1a-1.txt" io/resource))
  (-main file)

  (solve "HELLO")

  (seq "AAAAAAAAAAAAAAAAAAA")

  (def data (into [] data))
  data

  (every? (fn [v] (= v (get [\L \L \C] 0))) [\L \L \D])



  #dbg
   (defn solver [[curr & remain] answer]
     (if (empty? remain)
       (apply str answer)
       (let [all-same? (every? #(= % curr) remain)
             next (first remain)
             answer (conj answer curr)]
         (if all-same?
           (recur [nil] (conj answer remain))
           (if (<= curr next)
             (recur [(first remain) (rest remain)] (conj answer curr))
             (recur [(first remain) (rest remain)] answer))))))


  (solver [61 62] [])
  (empty? '())
  (rest '())
  (rest [62])

  (defn solver-2 [[first & remain] answer]
    (if (seq remain)
      (let [all-same? (every? #(= % first) remain)
            second (first remain)]
        all-same?)
      "end"))

  (solver-2 [61 62] [])



  (every? #(= % 61) '(61))

  (defn reducer [{:keys [data answer] :as state} idx]
    (if (= (inc idx) (count data))
      (apply str (conj answer (get data idx)))
      (let [curr (get data idx)
            next (get data (inc idx))
            answer (conj answer curr)]
        (if (< (int curr) (int next))
          (-> state
              (assoc :answer (conj answer curr)))
          (-> state
              (assoc :answer answer))))))


  (def data (->>
             (seq "HELLO")
             (into [])))
  data
  (apply str data)

  (reduce reducer {:data data :answer []} (range (count data)))

  (< (int (get data 1))
     (int (get data 0)))


  (solve "HELLOWORD")
  (->>
   (seq "HELLO")
   (into []))

  (if (> (int \H) (int \E))
    "big"
    "small")



  '())