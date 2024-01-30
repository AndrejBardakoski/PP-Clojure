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
  ([row] (uvs-horizontal-row row 1))
  ([row i]
   (cond
     (> i 9) row
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
    transponierte the matrix then apply `uvs-horizontal` then transponierte back the matrix"
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



;; ilegal-state?
;; ilegal state horizontal
(defn ilgl-sts-horizontal?
  "ilegal state horizontal
  returns true if a row in `matrix` has ilegal state
  ie. if in `matrix` there is a row where
  two or more sets has the same SINGLE value

  implementation:
  if filtered row with filter that returns only the sets that has single value
  has bigger value then then the set from filtered row then there are duolicates
  meaning the row is in ilegal state so return true"
  [matrix]
  (cond
    (empty? matrix) false
    (< (count (set (filter #(= 1 (count %)) (first matrix))))
       (count (filter #(= 1 (count %)) (first matrix))))
          true
    :else (ilgl-sts-horizontal? (rest matrix))))



;; ilegal state vertical
(defn ilgl-sts-vertical?
    "ilegal state? vertical
  returns true if a column in `matrix` has ilegal state
  ie. if in `matrix` there is a column where
  two or more sets has the same SINGLE value

implementation:
    transponierte the matrix then apply `ilgl-sts-horizontal?`"
  [matrix]
  (ilgl-sts-horizontal? (transpose matrix)))

;; ilegal state box
(defn ilgl-sts-box?
  "ilegal state? box
  returns true if a box-3x3 field in `matrix` has ilegal state
  ie. if in `matrix` there is a box where
  two or more sets has the same SINGLE value

  implementation:
  box-transform the matrix then apply `ilgl-sts-horizontal?`"
  [matrix]
  (ilgl-sts-horizontal? (box-transform matrix)))

(defn ilegal-state?
    "ilegal state?
  returns true if `matrix` has ilegal state
  ie. if in `matrix` there is a row, column, box where
  two or more sets has the same SINGLE value

  implementation:
  check if any row, column or box are in ilegal state separatly by appling the predicates:
  `ilgl-sts-horizontal?` `ilgl-sts-vertical?` `ilgl-sts-box?`"
  [matrix]
  (or
    (ilgl-sts-horizontal? matrix)
    (ilgl-sts-vertical? matrix)
    (ilgl-sts-box? matrix)
    )
  )


;; guessing
(defn find-guess-pos
  "returns the postition in `matrix` of the first set that has more than 1 element
  implementation:
  using `for` maps every set in `matrix` to its position in matrix or `nil` if it has only 1 element
  then filters the nil values and returns the first value"
  [matrix]
  (first
    (filter
      #(not(nil? %))
      (for [i (range 9) j (range 9)]
        (if (= 1 (count(nth (nth matrix i) j))) nil [i j])))))


(defn guess
  "depending on `mode` it eithder made a guess or remove a previos made guess
  if the mode is `:make-guess` finds the first set in matrix
  that has multiple values and sets it to the first value ie. makes a choise
  if the mode is anything else then remove a choise by finding the first set in matrix
  that has multiple values and sets it to the rest of the set ie. removes the first value from the set

  implementation:
  frist finds the position of the first set that has multiple element with `find-guess-pos`
  and than using nested `for` functions maps the sets in `matrix` to its value
  unless the set is at the previosly calculated position
  in that case aplies the function dictaded by the `mode`"
  ([matrix mode]
   (if (= mode :make-guess)
     (guess matrix #(list(first %)) (find-guess-pos matrix))
     (guess matrix rest (find-guess-pos matrix))))
  ([matrix mode pos]
   (for [i (range 9)]
     (if (= i (first pos))
        (for [j (range 9)]
           (if (= j (second pos))
              (set(mode(nth (nth matrix i) j )))
            (nth (nth matrix i) j)))
      (nth matrix i)
       )
     )))



;; solve
(defn solve
  "this is the main function
    first transforms the input `matrix` with the `transform function`
  then recursively applies `single-value-solver` and `unique-value-solver` to `matrix`
  until `matrix` is solved
  or until `matix` is in ilegal state
  or until `matrix` is equal as `matrix` in the previous iteration this means that the sudoku can't be solved just by
  appling the 'solver' functions we need to make a guess and if we were wrong backtrace the result
  so in this case we make a guess with `guess` function in `:make-guess` mode and continue to solve the SUDOKU
  if we get `nil` as result then the guess was wrong so we remove the guess from the sets of options
  with `guess` function in `:delete-guess` mode and continue to solve the SUDOKU"
  ([matrix] (do
              (println "input: SUDOKU")
              (clojure.pprint/print-table (range 9)matrix)
              (solve nil (transform matrix))))
  ([matrix_prev matrix]
   (cond
    (ilegal-state? matrix) nil
    (solved? matrix) (do
                       (println "output: solved SUDOKU")
                       (clojure.pprint/print-table (range 9) (inverse-transform matrix))
                       (println "") (println "") (println "")
                       (inverse-transform matrix))
    (= matrix_prev matrix)
       (or (solve matrix (guess matrix :make-guess))
                (solve matrix (guess matrix :delete-guess)))
    :else (recur matrix (unique-value-solver(single-value-solver matrix))))))
