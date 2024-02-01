;; transpose
(defn transpose
  "returns a transpose matrix of `matrix`

  implementation:
  using `for` to iterete through the lenght of the rows in `matrix`
  in each iteration get the n-th column of `matrix`"
  [matrix]
  (for [n (range (count (first matrix)))] (map #(nth % n) matrix)))

;; ilegal-state?
(defn ilegal-state?
  "returns true if `matrix` is in an ilegal state .ie if there are elements with same value in a column

  implementation:
  first transponse the `matrix` then map each row of the transposed matrix (column of the original)
  in true if the count of unique values is equal to the count of all values
  return true if at least one true is found"
  [matrix]
  (some false? (map #(= (count (set %))(count %))(transpose matrix))))



;; shift-R
(defn shift-R
  "returns `list` but right shifted for 1 position"
  [list]
  (cons (last list)(reverse (rest (reverse list)))))

;; solve
(defn solve
  "right shift the rows of `matrix` by 1 until there are no duplicate values in the columns

  implementation:
  first add the first row of `matrix` to the aggregator and set the iterator to 0

  recursively iterate the `matrix` depending on the state,
  if the first row together with the aggregator is in ilegal state
  then shift the row and increase the iterator and continue with iteration
  if it is not in ilegal state then if
    adding the row to the aggregator, reseting the iterator
    and continuing with the iteration with the rest of matrix
    gives result, return it, else continue shifting the row
  if the iterator reach the lenght of the row then return `nil`
  if all the rows are added in the aggregator ie. `matrix` is empty return the aggregator"
  ([matrix] (solve (rest matrix) (list(first matrix)) 0))
  ([matrix agg i]
   (cond
    (empty? matrix) (reverse agg)
    (= i (count (first matrix))) nil
    (ilegal-state? (cons (first matrix) agg))
       (recur (cons (shift-R (first matrix))(rest matrix)) agg (inc i))
    :else
       (or (solve (rest matrix) (cons (first matrix) agg) 0)
           (solve (cons (shift-R (first matrix))(rest matrix)) agg (inc i))))))


;; show
(defn show-output [matrix]
  (if matrix
    (clojure.pprint/print-table (range (count (first matrix))) (map vec matrix))
    (println "Unsolvable")))

(defn show
  [matrix]
  (do
    (println "input: boja")
    (clojure.pprint/print-table (range (count (first matrix))) (map vec matrix))
    (println "") (println "") (println "")
    (println "output:") (show-output (solve matrix))))
