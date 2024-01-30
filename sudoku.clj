;; #SUDOKU

;; ##transform
(defn transform-row
  "helper function for `transform`
  transforms the input vector into vector of sets

  for each element in `row`
  if the element is `0` map it into set of all numbers 1-9
  else map the element into set of it self example `3->#{3}`"
  [row]
  (cond
    (empty? row) []
    (= 0 (first row)) (vec (cons (set (range 1 10)) (transform-row (rest row))))
    :else (vec (cons (set(list(first row))) (transform-row (rest row))))))

(defn transform
  "transfroms `matrix` into matrix of sets

  for each row apply
  the helper function `transform-row`"
  [matrix]
  (cond
    (empty? matrix) []
    :else (vec (cons (transform-row (first matrix))(transform (rest matrix))))))


;; ##inverse transform
(defn invers-trans-row
  "helper function for `inverse-transform`
  transforms the input vector of sets into vector

  maps each element in `row` by extracting the first and only element in the set"
  [row]
  (cond
    (empty? row) []
    :else (vec (cons (first(first row))(invers-trans-row (rest row))))))

(defn inverse-transform
  "transfroms the solved matrix of sets into matrix

  for each row apply
  the helper function `inverse-trans-row`"
  [matrix]
  (cond
    (empty? matrix) []
    :else (vec (cons (invers-trans-row (first matrix))(inverse-transform (rest matrix))))))


;; ##solved?
(defn solved-row?
  "helper function for `solved?`
  return true if each set in row has only 1 value,ie the row is solved "
  [row]
  (cond
    (empty? row) true
    (= 1 (count (first row))) (solved-row? (rest row))
    :else false))

(defn solved?
  "return true if each set in each column has only 1 value,ie the matrix is solved
  for each row apply
  the helper function `solved-row?`
  "
  [matrix]
  (cond
    (empty? matrix) true
    (solved-row? (first matrix)) (solved? (rest matrix))
    :else false))


;; ##single-value-solver
;; ###single-value-solver-horizontal
(defn set-member?
  "return true if `x` is member of the set `s`"
  [s x] (some (partial = x) s))

(defn rmv-e-from-sets
  "removes element `e` from sets in the vector of sets `row`
  only removes it if the set contains multiple elements to avoid empty sets"
  [row e]
  (cond
    (empty? row) ()
    (= 1(count (first row))) (cons (first row)(rmv-e-from-sets (rest row) e))
    (set-member? (first row) e)
     (cons (set (filter #(not (= e %)) (first row) ))(rmv-e-from-sets (rest row) e))
    :else (cons (first row)(rmv-e-from-sets (rest row) e))
    ))

(defn svs-horizontal-row
    "helper function for `svs-horizontal`
  if in the `row` there is a set with a single value
  removes the value from all other sets in the `row`  "
  ([row] (svs-horizontal-row row 0))
  ([row iterator]
   (cond
    (= (count row) iterator) row
    (= 1(count (nth row iterator)))
     (svs-horizontal-row (rmv-e-from-sets row (first (nth row iterator))) (inc iterator))
    :else (svs-horizontal-row row (inc iterator)))))

(defn svs-horizontal
  "single value solver horizontal
  if in the `matrix` there is a set with a single value
  removes the value from all other sets in the same row

  implementation:
  for each row apply
  the helper function `svs-horizontal-row`"
  [matrix]
  (cond
    (empty? matrix) ()
    :else (cons (svs-horizontal-row (first matrix))(svs-horizontal (rest matrix)))))


;; ###single-value-solver-vertical
(defn get-nth-column
  "returns the `n`-th column of the `matrix`

  implementation:
  take nth element from each row"
  [matrix n]
  (cond
    (empty? matrix) ()
    :else (cons (nth (first matrix) n)(get-nth-column (rest matrix) n))))

(defn transpose
  "returns a transpose matrix of `matrix`

  implementation:
  iterate for the length of the matrix
  and in each iteration add the n-th column to an aggregator"
  ([matrix] (reverse(transpose matrix [] 0)))
  ([matrix agr i]
   (cond
     (= (count matrix) i) agr
     :else (transpose matrix (cons (get-nth-column matrix i) agr) (inc i)))))

(defn svs-vertical
  "single value solver vertical
  if in the `matrix` there is a set with a single value
  removes the value from all other sets in the same column

  implementation:
  transpose the matrix then apply `svs-horizontal` then transpose back the matrix"
  [matrix]
  (transpose (svs-horizontal (transpose matrix))))


;; ###single-value-solver-box
(defn get-nth-box
    "returns the `n`-th box 3x3 field of the `matrix`

  implementation:
  map `n` into n= n/3 and m=n mod3
  then return all elements from (3*n-th to 3*(n+1)-th)row and (3*m-th to 3*(m+1)-th)column
  "
  ([matrix n] (get-nth-box matrix (quot n 3)(rem n 3)))
  ([matrix n m]
  (for [i (range (* 3 n) (* 3 (inc n))) j (range (* 3 m) (* 3 (inc m)))]
    (nth (nth matrix i) j))))

(defn box-transform
    "returns a box-transformed `matrix`
  the n-th 3x3 field from the matrix bocomes the n-th row of the new matrix

  implementation:
  iterate for the length of the matrix
  and in each iteration add the n-th box to an aggregator"
([matrix] (reverse(box-transform matrix [] 0)))
  ([matrix agr i]
   (cond
     (<= 9 i) agr
     :else (box-transform matrix (cons (get-nth-box matrix i) agr) (inc i)))))

(defn svs-box
    "single value solver box
  if in the `matrix` there is a set with a single value
  removes the value from all other sets in the same box - 3x3 field

  implementation:
  box-transform the matrix then apply `svs-horizontal` then box-transform back the matrix
  `box-transform is inverse to itself so there is no need for special inverse-box-transform function`"
  [matrix]
  (box-transform (svs-horizontal (box-transform matrix))))

(defn single-value-solver
  "single value solver
  applies `svs-horizontal` `svs-vertical` `svs-box` to `matrix` and returns the result"
  [matrix]
  (svs-box(svs-vertical (svs-horizontal matrix))))



;; ##unique-value-solver
;; ###unique-value-solver-horizontal
(defn uvs-horizontal-row
  "helper function for `uvs-horizontal`
  if in the `row` there is a value that is present in only single set then
  changes the set to contain only that value and
  removes the value from all other sets in the `row`

  implementation:
  iterate for each number in range [1-9]
  filter the sets in the `row` that the number apears in
  count the remaining set, if it is only 1 then use `map` to apply the rule and continue
  with the iteration"
  ([row] (uvs-horizontal-row row 0))
  ([row i]
   (cond
     (= i (count row)) row
     (= 1 (count (filter #(set-member? % i) row)))
       (uvs-horizontal-row
         (map
           #(if (set-member? % i)
              (set(list i))
              % )
           row)
         (inc i))
     :else (uvs-horizontal-row row (inc i)))))

(defn uvs-horizontal
    "unique value solver horizontal
  if in the `matrix` there is a value that is present in only single set in the same row
  then changes the set to contain only that value and
  removes the value from all other sets in the row

  implementation:
  for each row apply
  the helper function `uvs-horizontal-row`"
  [matrix]
(cond
    (empty? matrix) ()
    :else (cons (uvs-horizontal-row (first matrix))(uvs-horizontal (rest matrix)))))

;; ##unique-value-solver-vertical
(defn uvs-vertical
  "unique value solver vertical
  if in the `matrix` there is a value that is present in only single set in the same column
  then changes the set to contain only that value and
  removes the value from all other sets in the same column

  implementation:
    transpose the matrix then apply `uvs-horizontal` then transpose back the matrix"
  [matrix]
  (transpose (uvs-horizontal (transpose matrix))))

;; ##unique-value-solver-box
(defn uvs-box
  "unique value solver box
  if in the `matrix` there is a value that is present in only single set in the same box
  then changes the set to contain only that value and
  removes the value from all other sets in the same box - 3x3 field

  implementation:
  box-transform the matrix then apply `uvs-horizontal` then box-transform back the matrix"
  [matrix]
  (box-transform (uvs-horizontal (box-transform matrix))))


(defn unique-value-solver
    "unique value solver
  applies `uvs-horizontal` `uvs-vertical` `uvs-box` to `matrix` and returns the result"
  [matrix]
  (uvs-box(uvs-vertical (uvs-horizontal matrix))))


(defn solve
  "the main function
    first transforms the input `matrix` with the `transform function`
  then recursively applies `single-value-solver` and `unique-value-solver` to `matrix`
  until `matrix` is solved
  or until `matrix` is equal as `matrix` in the previous iteration this can happen for multiple reasons:
    -the sudoku is unsolvable
    -the sudoku contains multiple correct solutions
    -the input was wrong
    -the sudoku can't be solved using this algorithm it requires more advance logic
        like to look at pairs or triples of cells within a row, column, or block"
  ([matrix] (solve nil (transform matrix)))
  ([matrix_prev matrix]
   (cond
    (solved? matrix) (inverse-transform matrix)
    (= matrix_prev matrix) matrix_prev      ;;"unsolvable"
    :else (solve matrix (unique-value-solver(single-value-solver matrix))))))
