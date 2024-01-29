(defn solve
  ([matrix] (solve nil (transform matrix)))
  ([matrix_prev matrix]
   (cond
    (solved? matrix) (inverse-transform matrix)
    (= matrix_prev matrix)
     matrix
;;      "unsolvable"
    :else (solve matrix (unique-value-solver(single-value-solver matrix))))))


(defn transform [matrix]
  (cond
    (empty? matrix) []
    :else (vec (cons (transform-row (first matrix))(transform (rest matrix))))))

(defn transform-row [row]
  (cond
    (empty? row) []
    (= 0 (first row)) (vec (cons (set (range 1 10)) (transform-row (rest row))))
    :else (vec (cons (set(list(first row))) (transform-row (rest row))))))


(defn inverse-transform [matrix]
  (cond
    (empty? matrix) []
    :else (vec (cons (invers-trans-row (first matrix))(inverse-transform (rest matrix))))))

(defn invers-trans-row [row]
  (cond
    (empty? row) []
    :else (vec (cons (first(first row))(invers-trans-row (rest row))))))

(defn solved? [matrix]
  (cond
    (empty? matrix) true
    (solved-row? (first matrix)) (solved? (rest matrix))
    :else false))

(defn solved-row? [row]
  (cond
    (empty? row) true
    (= 1 (count (first row))) (solved-row? (rest row))
    :else false))



(defn single-value-solver [matrix]
  (svs-box(svs-vertical (svs-horizontal matrix))))

(defn svs-horizontal [matrix]
  (cond
    (empty? matrix) ()
    :else (cons (svs-horizontal-row (first matrix))(svs-horizontal (rest matrix)))))

(defn svs-horizontal-row
  ([row] (svs-horizontal-row row 0))
  ([row iterator]
   (cond
    (= (count row) iterator) row
    (= 1(count (nth row iterator)))
     (svs-horizontal-row (rmv-e-from-sets row (first (nth row iterator))) (inc iterator))
    :else (svs-horizontal-row row (inc iterator)))))

(defn rmv-e-from-sets [row e]
  (cond
    (empty? row) ()
    (= 1(count (first row))) (cons (first row)(rmv-e-from-sets (rest row) e))
    (set-member? (first row) e)
     (cons (set (filter #(not (= e %)) (first row) ))(rmv-e-from-sets (rest row) e))
    :else (cons (first row)(rmv-e-from-sets (rest row) e))
    ))

(defn set-member? [s x] (some (partial = x) s))

(defn svs-vertical [matrix]
  (transpose (svs-horizontal (transpose matrix))))

(defn transpose
  ([matrix] (reverse(transpose matrix [] 0)))
  ([matrix agr i]
   (cond
     (= (count matrix) i) agr
     :else (transpose matrix (cons (get-nth-column matrix i) agr) (inc i)))))

(defn get-nth-column [matrix n]
  (cond
    (empty? matrix) ()
    :else (cons (nth (first matrix) n)(get-nth-column (rest matrix) n))
    )
  )


(defn svs-box [matrix]
  (box-transform (svs-horizontal (box-transform matrix))))

(defn box-transform
([matrix] (reverse(box-transform matrix [] 0)))
  ([matrix agr i]
   (cond
     (<= 9 i) agr
     :else (box-transform matrix (cons (get-nth-box matrix i) agr) (inc i)))))

(defn get-nth-box
  ([matrix n] (get-nth-box matrix (quot n 3)(rem n 3)))
  ([matrix n m]
  (for [i (range (* 3 n) (* 3 (inc n))) j (range (* 3 m) (* 3 (inc m)))]
    (nth (nth matrix i) j))))

(defn unique-value-solver [matrix]
  (uvs-box(uvs-vertical (uvs-horizontal matrix))))

(defn uvs-box [matrix]
  (box-transform (uvs-horizontal (box-transform matrix))))

(defn uvs-vertical [matrix]
  (transpose (uvs-horizontal (transpose matrix))))

(defn uvs-horizontal [matrix]
(cond
    (empty? matrix) ()
    :else (cons (uvs-horizontal-row (first matrix))(uvs-horizontal (rest matrix)))))

(defn uvs-horizontal-row
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
