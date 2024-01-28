;; 1.0
(defn atomic?
  "check if `v` is atomic
  use the clojure predicate coll? to check if `v` is collection
  if it is collection then it is not atomic returns `false`
  and if it is not collection than it is atomic returns `true`"
  [v]
  (not (coll?  v)))


;; 1.1
(defn member?
  "check if `x` is member of `lst`

  iterate `lst` recursively until:
    -element that is equal to `x` is found in this case return `true`
    -the whole list is iterated in this case return `false`"
  [x lst]
  (cond
    (empty? lst) false
    (= x (first lst)) true
    :else (member? x (rest lst))))


;; 1.2
(defn my-count
  "return the length of `lst`

  retunrs increment of the lenght of `lst` without the first element
  in other words iterate the whole `lst` recursively and in each iteration add 1"
  [lst]
  (cond
    (empty? lst) 0
    :else (inc (my-count (rest lst)))))


;; 1.3
(defn append
  "return concatenation of `lst1` and `lst2`

  iterate `lst1` recursively and add each element in the result
  when the whole 'lst1' is iterated add `lst2` in the result"
  [lst1 lst2]
  (cond
    (empty? lst1) lst2
    :else (cons (first lst1)(append (rest lst1) lst2))))


;; 1.4
(defn zip
  "return list of pairs when the 1st element in the pair comes from `lst1` and the 2nd from `lst2 accordingly`

  iterate `lst1` anf `lst2` simuntaneously until at least on of them is empty
  in each iteration add pair of element from 'lst1' and from 'lst2' in the result"
  [lst1 lst2]
  (cond
    (empty? lst1) ()
    (empty? lst2) ()
    :else (cons (list (first lst1) (first lst2))(zip (rest lst1) (rest lst2)))))


;; 1.5
(defn lookup
  "return the value assigned to `key` in `list-of-pairs`

  iterate 'list-of-pairs' recursively until:
    -pair where the first element is equal to `key` is found in this case return the second element in the pair
    -the whole list is iterated in this case return `nil`"
  [key list-of-pairs]
  (cond
    (empty? list-of-pairs) nil
    (= key (first (first list-of-pairs))) (second (first list-of-pairs))
    :else (lookup key (rest list-of-pairs))))


;; 1.6
(defn my-merge [lst1 lst2]
  "merge sorted lists `lst1` and `lst2` in a way that the result is also sorted list

  iterate `lst1` and `lst2` recursively  if the element from 'lst1' is bigger
  add the element from `lst2` in the result then continue the iteration with `lst1` and rest of `lst2`
  and vice versa if the element from `lst2` is bigger add the element from 'lst1' in the result
  and continue the iteration with `lst2` and rest of 'lst1'.
  when you reach the end in one of the lists add the other to the result"
  (cond
    (empty? lst1) lst2
    (empty? lst2) lst1
    (> (first lst1) (first lst2)) (cons (first lst2)(my-merge lst1 (rest lst2)))
    :else (cons (first lst1)(my-merge (rest lst1) lst2))))


;; 1.7
(defn count-all
  "reteurn the number of atomic elements in `lst` regardless of the list level they belong

  iterate `lst` if the current element is atomic add 1 to the result
  else return the sum of count-all of the current element and count-all of the rest of 'lst'
  "
  [lst]
  (cond
    (empty? lst) 0
    (atomic? (first lst)) (inc (count-all (rest lst)))
    :else (+ (count-all (first lst))(count-all (rest lst)))))


;; 1.8
(defn my-drop
  "return `lst` without the first `n` elements

  iterate `lst` while decresing `n` until `n` reachs 0 then return what's left from `lst`"
  [n lst]
  (cond
    (= n 0) lst
    (empty? lst) ()
    :else (my-drop (dec n)(rest lst))))


;; 1.9
(defn my-take
  "return the first `n` elements from `lst`

  iterate `lst` while decresing `n` and add the current element in the result
  when `n` reachs 0 return () - skipping the rest from `lst`"
  [n lst]
  (cond
    (= n 0) ()
    (empty? lst) ()
    :else (cons (first lst)(my-take (dec n)(rest lst)))))


;; 1.10
(defn my-reverse
  "return `lst` reversed

  first call my-reverse with additional argument `agg`=() that will have the role of aggregator
  then iterate 'lst' and add each element in the aggregator when you iterate the whole list return `agr`"
  ([lst] (my-reverse lst ()))
  ([lst agr]
   (cond
    (empty? lst) agr
    :else (my-reverse (rest lst)(cons (first lst) agr)))))


;; 1.11
(defn remove-duplicates
  "return `lst` with uniqu elements

  ietrate `lst` and for each element check if it is member of the rest of 'lst'
  if it is then skip it (don't add it to the result)
  else add it in the result"
  [lst]
   (cond
    (empty? lst) ()
    (member? (first lst) (rest lst)) (remove-duplicates (rest lst))
    :else (cons (first lst) (remove-duplicates (rest lst)))))


;; 1.12
(defn my-flatten
  "return `list-of-lists` with one level of parentheses removed

  iterate `lst` if the current element is atomic add it in the result using `cons`
  else add it in the result using our `append` function equivalent to clojure `concat`
  "
  [list-of-lists]
  (cond
    (empty? list-of-lists)  ()
    (atomic? (first list-of-lists)) (cons (first list-of-lists) (my-flatten (rest list-of-lists)))
    :else (append (first list-of-lists)(my-flatten (rest list-of-lists)))
    ))



;; 2.0
(defn buzz
  "return `list-of-ints` but all numbers containing 7 or divisible by 7 are replaced with :buzz

  uses the map function with custum made mapping function
  the mapping fungtion return :buzz if the argument is divisible by 7 or contains 7
  return the argument unchanged otherwise
  to check if the argument contains 7, the argument is transformed in list of chars with `(seq (str %))`
  then the truth is calculated using the function `some` with `(partial = \7)` as predicate"
  [list-of-ints]
  (map
    #(cond
      (= 0 (rem % 7)) :buzz
      (some (partial = \7) (seq (str %))) :buzz
      :else %)
    list-of-ints))


;; 2.1
(defn divisors-of
  ""

  [n]
  (filter #(= 0 (mod n %)) (range 2 n)))


;; 2.2
(defn longest [list-of-strings]
  (reduce
    #(if (>= (count %1) (count %2))
       %1
       %2)
    list-of-strings))



;; 3.0
(defn my-map [f lst]
  (cond
    (empty? lst) ()
    :else (cons (f (first lst))(my-map f (rest lst)))))


;; 3.1
(defn my-filter [pred lst]
  (cond
    (empty? lst) ()
    (pred (first lst)) (cons (first lst)(my-filter pred (rest lst)))
    :else (my-filter pred (rest lst))))


;; 3.2
(defn my-reduce
  ([f lst] (my-reduce f (first lst) (rest lst)))
  ([f value? lst]
   (cond
     (empty? lst) value?
     :else (my-reduce f (f value? (first lst)) (rest lst)))))


;; 3.3
(defn my-flat-map "documentation string" [f lst]
  (cond
    (empty? lst) ()
    :else (append (f (first lst)) (my-flat-map f (rest lst)))))
