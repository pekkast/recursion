(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [val (first a-seq)
          elems (rest a-seq)]
      (if (empty? elems)
        val
        (max val (max-element elems))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [val (first a-seq)
          filtered (my-filter pred? (rest a-seq))]
      (if (pred? val)
        (cons val filtered)
        filtered))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    ((complement =) (count a-seq) (count b-seq)) false
    (and (empty? a-seq) (empty? b-seq)) true
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== 0 k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (== 0 n) 0
    (== 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '() (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (let [next (- up-to 1)]
    (if (< next 0) '() (cons next (my-range next)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(()) (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq) '(()) (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotate-until-same [a-seq, orig]
  (let [next (concat (rest a-seq) [(first a-seq)])]
    (if (= next orig)
      '()
      (concat [next] (rotate-until-same next orig)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (concat [a-seq] (rotate-until-same a-seq a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [el (first a-seq)
          left (filter #(not (= el %)) a-seq)]
      (my-frequencies-helper (assoc freqs el (- (count a-seq) (count left))) left))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[el num] (first a-map)]
      (concat (repeat num el) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (if (empty? a-seq)
    []
    (let [num (int (/ (count a-seq) 2))]
      (concat [(my-take num a-seq)] [(my-drop num a-seq)]))))

(defn add-element [pred? el a-seq]
  (concat (my-take-while pred? a-seq) [el] (my-drop-while pred? a-seq)))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [el (first a-seq)]
            (seq-merge (rest a-seq) (add-element #(< % el) el b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[fst snd] (halve a-seq)]
      (seq-merge (merge-sort fst) (merge-sort snd)))))

(defn take-monotonic [a-seq list cmp?]
  (if (empty? a-seq)
    (reverse list)
    (let [next (first a-seq)
          prev (first list)] (if (cmp? next prev)
                               (reverse list)
                               (take-monotonic (rest a-seq) (cons next list) cmp?)))))

(defn split-into-monotonics [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [el (first a-seq)
          gte (take-monotonic (rest a-seq) [el] (fn [next prev] (< next prev)))
          lte (take-monotonic (rest a-seq) [el] (fn [next prev] (> next prev)))
          part (if (> (count gte) (count lte)) gte lte)]
      (concat [part] (split-into-monotonics (drop (count part) a-seq))))))

(defn permutations [a-set]
  (if (<= (count a-set) 1)
    (cons a-set '())
    (apply concat (map (fn [el] (map (fn [perm] (concat [el] perm)) (permutations (filter #(not (= el %)) a-set)))) a-set))))

(defn powerset-helper [res a-set]
  (if (or (empty? a-set) (some #(= a-set %) res))
    res
    (let [cur (concat [a-set] res)]
      (apply concat res (map (fn [el] (powerset-helper cur (filter #(not (= el %)) a-set))) a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    [a-set]
    (powerset-helper #{{}} a-set)))
