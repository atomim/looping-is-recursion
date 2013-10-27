(ns looping-is-recursion)

(defn power [base exp]
  (let [helper
        (fn [acc base exp]
          (if (pos? exp)
            (recur (* acc base) base (dec exp))
            acc))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  (if (= (first seq1) (first seq2))
    (if (and (empty? seq1) (empty? seq2))
      true
      (if (or (empty? seq1) (empty? seq2))
        false
        (recur (rest seq1) (rest seq2))
      )
     )false))

(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq
         ]
    (if (empty? s)
      nil
      (if (pred (first s))
        i
        (recur (inc i) (rest s))))))

(defn avg [a-seq]
  (loop [sum 0 i 0 s a-seq]
    (if (empty? s)
      (/ sum i)
      (recur (+ sum (first s)) (inc i) (rest s)))))


(defn parity [a-seq]
  (loop [odd #{} s a-seq]
    (if (empty? s)
      odd
      (if (contains? odd (first s))
        (recur (disj odd (first s)) (rest s))
        (recur (conj odd (first s)) (rest s))
     ))
    ))

(defn fast-fibo [n]
  (loop [p1 0 p2 1 i n]
    (if (pos? i)
      (recur p2 (+ p1 p2) (dec i))
      p1)))

(defn cut-at-repetition [a-seq]
  (loop [past #{(first a-seq)} s (rest a-seq) res [(first a-seq)]]
    (if (or (empty? s) (contains? past (first s)))
      res
      (recur (conj past (first a-seq)) (rest s) (conj res (first s))))))

