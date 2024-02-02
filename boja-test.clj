(ns user (:use clojure.test))


;; transpose
(deftest test-transpose
  (is (= '((1)) (transpose '((1)))))
  (is (= '((1)(2)(3)(4)) (transpose '((1 2 3 4)))))
  (is (= '((1 2 3)(1 2 3)) (transpose '((1 1)(2 2)(3 3)))))
  (is (= '((1 4 7)(2 5 8)(3 6 9)) (transpose '((1 2 3)(4 5 6)(7 8 9))))))


;; ilegal-state?
(deftest test-ilegal-state?
  (is (= true (ilegal-state? '((1 2 3)(1 2 3)))))
  (is (= true (ilegal-state? '((1 2 3)(4 5 3)))))
  (is (= true (ilegal-state? '((1 1 1)(2 2 2)(3 3 1)))))
  (is (= nil (ilegal-state? '((1 1 1)(2 2 2)(3 3 3)))))
  (is (= nil (ilegal-state? '((1 2 3)(3 1 2)(2 3 1))))))

;; shift-R
(deftest test-shift-R
  (is (= '(3 1 2) (shift-R '(1 2 3))))
  (is (= '(5 1 2 3 4) (shift-R '(1 2 3 4 5))))
  (is (= '(1 1 2 3 2) (shift-R '(1 2 3 2 1)))))


;; solve
(deftest test-solve
  (is (= '((1 2 3)) (solve '((1 2 3)))))
  (is (= '((1 2 3)(3 4 5)) (solve '((1 2 3)(4 5 3)))))
  (is (= nil (solve '((1 1 1)(2 2 2)(3 3 1)))))
  (is (= '((1 1 1)(2 2 2)(3 3 3)) (solve '((1 1 1)(2 2 2)(3 3 3)))))
  (is (= '((1 2 3)(3 1 2)(2 3 1)) (solve '((1 2 3)(1 2 3)(1 2 3)))))
  (is (= '((1 2 3)(2 3 4)(3 4 5)(4 5 2)(5 1 1)) (solve '((1 2 3)(2 3 4)(3 4 5)(4 5 2)(1 5 1)))))
  (is (= '((1 2 3 2 1)(3 3 1 1 2)(2 1 2 3 3)) (solve '((1 2 3 2 1)(1 1 2 3 3)(3 3 2 1 2)))))
  (is (= '((1 1 1 2 1)(2 2 2 3 2)(3 3 3 4 3)(4 4 4 1 4)) (solve '((1 1 1 2 1)(2 2 3 2 2)(3 3 3 3 4)(4 1 4 4 4)))))
  (is (= nil (solve '((1 2 3 2 1)(1 2 3 1 3)))))
  (is (= '((1 2 3 2 1)(4 5 1 4 5)(3 1 5 5 4)(5 4 4 1 3)(2 3 2 3 2)) (solve '((1 2 3 2 1)(4 5 4 5 1)(4 3 1 5 5)(3 5 4 4 1)(2 2 3 2 3))))))


(run-tests)
