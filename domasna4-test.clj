(ns user (:use clojure.test))


;; 1.0
(deftest test-atomic?
  (is (= true (atomic? 2)))
  (is (= true (atomic? :2)))
  (is (= true (atomic? \2)))
  (is (= true (atomic? "2")))
  (is (= false (atomic? '(2 3))))
  (is (= false (atomic? [1 2 3])))
  (is (= false (atomic? #{1 2 3}))))

;; 1.1
(deftest test-member?
  (is (= false (member? 2 ())))
  (is (= false (member? 2 '(1 3 4 5))))
  (is (= false (member? 2 '(5 4 3 (2) 1))))
  (is (= true (member? 2 '(1 2 3 4 5))))
  (is (= true (member? :x '(1 :a 3 :x 5)))))

;; 1.2
(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(1))))
  (is (= 1 (my-count '((1)))))
  (is (= 5 (my-count '(1,2,3,4,5))))
  (is (= 4 (my-count '((1),2,(3 (4) 5 )(6))))))

;; 1.3
(deftest test-append
  (is (= () (append ()())))
  (is (= '(1 2 3) (append '(1 2 3)())))
  (is (= '(1 2 3) (append ()'(1 2 3))))
  (is (= '(1 2 3) (append '(1)'(2 3))))
  (is (= '(1 (2) 3 (4 5)) (append '(1 (2) 3)'((4 5))))))

;; 1.4
(deftest test-zip
  (is (= () (zip ()())))
  (is (= () (zip ()'(1 2 3))))
  (is (= '((1 :a)(2 :b)) (zip '(1 2)'(:a :b))))
  (is (= '((1 :a)(2 :b)) (zip '(1 2)'(:a :b))))
  (is (= '((1 :a)(2 :b)(3 :c)((4) :d)) (zip '(1 2 3 (4) (5 6))'(:a :b :c :d)))))

;; 1.5
(deftest test-lookup
  (is (= nil (lookup :x '())))
  (is (= nil (lookup :x '((:a 2) (:b 4) (:c 6)))))
  (is (= nil (lookup :x '((1 :x) (2 :y) (3 :z)))))
  (is (= 2 (lookup :x '((:a 1) (:x 2) (:c 3)))))
  (is (= '(2 3) (lookup :x '((:a 1) (:x (2 3)) (:c 4)))))
  (is (= :x (lookup '(1 2) '(((1 2) :x) (3 :y) (4 6))))))

;; 1.6
(deftest test-my-merge
  (is (= () (my-merge ()())))
  (is (= '(1 2 3) (my-merge '(1 2 3)())))
  (is (= '(1 2 3) (my-merge ()'(1 2 3))))
  (is (= '(1 2 3 3 4 4) (my-merge '(1 3 4)'(2 3 4))))
  (is (= '(1 2 2 5 6 10 12 13 22) (my-merge '(1 2 5 6 10 13)'(2 12 22)))))

;; 1.7
(deftest test-count-all
  (is (= 0 (count-all ())))
  (is (= 3 (count-all '(1 2 3))))
  (is (= 3 (count-all '((1 2) 3))))
  (is (= 4 (count-all '((1 3) (3 () (2))))))
  (is (= 3 (count-all '(() () (1 2 3 ()))))))

;; 1.8
(deftest test-my-drop
  (is (= () (my-drop 2 '())))
  (is (= () (my-drop 2 '(1 2))))
  (is (= () (my-drop 4 '(1 2))))
  (is (= '(3 4 5) (my-drop 2 '(1 2 3 4 5))))
  (is (= '(3 (4 5)) (my-drop 2 '(1 2 3 (4 5))))))

;; 1.9
(deftest test-my-take
  (is (= () (my-take 2 '())))
  (is (= '(1 2) (my-take 2 '(1 2))))
  (is (= '(1 2) (my-take 4 '(1 2))))
  (is (= '(1 2) (my-take 2 '(1 2 3 4 5))))
  (is (= '((1 2)) (my-take 1 '((1 2) 3 (4 5))))))

;; 1.10
(deftest test-my-reverse
  (is (= () (my-reverse '())))
  (is (= '(2 1) (my-reverse '(1 2))))
  (is (= '(1 2 3) (my-reverse '(3 2 1))))
  (is (= '(4 (2 3) 1) (my-reverse '(1 (2 3) 4))))
  (is (= '((6 7) 5 4 (1 2 3)) (my-reverse '((1 2 3) 4 5 (6 7))))))

;; 1.11
(deftest test-remove-duplicates
  (is (= () (remove-duplicates '())))
  (is (= '(1 2 3) (remove-duplicates '(1 2 3))))
  (is (= '(1) (remove-duplicates '(1 1 1))))
  (is (= '(2 1 4 3) (remove-duplicates '(1 2 3 2 3 4 1 4 3))))
  (is (= '((1) (1 2) 1 2) (remove-duplicates '(1 (1) (1 2) 1 2)))))

;; 1.12
(deftest test-my-flatten
  (is (= () (my-flatten '())))
  (is (= '(1 2 3) (my-flatten '(1 2 3))))
  (is (= '(1 2 3 4) (my-flatten '(1 (2 3) (4)))))
  (is (= '(1 (2) 3 4) (my-flatten '((1 (2) 3) 4))))
  (is (= '((1)) (my-flatten '(((1)))))))

;; 2.0
(deftest test-buzz
  (is (= () (buzz '())))
  (is (= '(1 2 3) (buzz '(1 2 3))))
  (is (= '(1 :buzz 1) (buzz '(1 7 1))))
  (is (= '(1 :buzz :buzz) (buzz '(1 70 172))))
  (is (= '(1 :buzz :buzz :buzz 23 :buzz) (buzz '(1 17 14 71 23 28)))))

;; 2.1
(deftest test-divisors-of
  (is (= () (divisors-of 1)))
  (is (= '() (divisors-of 5)))
  (is (= '(2 5) (divisors-of 10)))
  (is (= '(2 3 4 6) (divisors-of 12)))
  (is (= '(2 3 5 6 9 10 15 18 30 45) (divisors-of 90))))

;; 2.2
(deftest test-longest
  (is (= "str" (longest '("str"))))
  (is (= "str" (longest '("str" "2" "5"))))
  (is (= "str" (longest '("str" "123" "v"))))
  (is (= "1234" (longest '("str" "1234" "haha"))))
  (is (= "str" (longest '("" "1" "23" "str")))))

;; 3.0
(deftest test-my-map
  (is (= () (my-map (partial + 2) ())))
  (is (= '(3 4) (my-map (partial + 2) '(1 2))))
  (is (= '(2 4 6 8) (my-map (partial * 2) '(1 2 3 4))))
  (is (= '(true false true false) (my-map odd? '(1 2 3 4))))
  (is (= '(3 7 11) (my-map #(+ (first %) (second %)) '((1 2) (3 4) (5 6))))))

;; 3.1
(deftest test-my-filter
  (is (= () (my-filter (partial = 2) ())))
  (is (= '(2) (my-filter (partial = 2) '(1 2))))
  (is (= '(3 4) (my-filter (partial < 2) '(1 2 3 4))))
  (is (= '(1 3 5) (my-filter odd? '(1 2 3 4 5 6))))
  (is (= '((3 3) (4 4)) (my-filter #(= (first %) (second %)) '((1 2) (3 3) (4 4) (5 6))))))

;; 3.2
(deftest test-my-reduce
  (is (= 2 (my-reduce = '(2))))
  (is (= 3 (my-reduce + '(1 2))))
  (is (= 24 (my-reduce * '(1 2 3 4))))
  (is (= 100 (my-reduce * '(1 2 5 10))))
  (is (= '(13 15) (my-reduce #(list (+ (first %1) (first %2))(+ (second %1) (second %2))) '((1 2) (3 3) (4 4) (5 6))))))

;; 3.3
(deftest test-my-flat-map
  (is (= () (my-flat-map list ())))
  (is (= '(1 2) (my-flat-map list '(1 2))))
  (is (= '(\1 \2 \2 \3 \3 \3 \4 \4 \1) (my-flat-map seq '("1" "22" "333" "441"))))
  (is (= '(1 2 3 2 3 4 3 4 5) (my-flat-map  #(list % (inc %) (inc(inc %))) '(1 2 3))))
  (is (= '(1 (2 3) :a (:b) (:x :x) :y) (my-flat-map #(list (first %) (second %)) '((1 (2 3) 4 5) (:a (:b) :c ) ((:x :x) :y)))))
  (is (= '(1 2 :a :b :x :x) (my-flat-map #(list (first %) (second %)) '((1 2 3 4 5) (:a :b :c ) (:x :x :y))))))

(run-tests)