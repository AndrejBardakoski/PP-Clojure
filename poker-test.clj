(ns user (:use clojure.test))

(def high-seven ["2H" "3S" "4C" "5C" "7D"])
(def high-ace ["9H" "3S" "4C" "5C" "AD"])
(def pair-hand ["2H" "4S" "4C" "5C" "7D"])
(def pair-7-hand ["2H" "4S" "7C" "5C" "7D"])
(def two-pairs-hand ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])



;;1 suit
(deftest test1-suit
  (is (= "C" (suit "TC")))
  (is (= "D" (suit "4D")))
  (is (= "H" (suit "6H")))
  (is (= "S" (suit "7S")))
  (is (= "S" (suit "AS"))))

;;2 rank
(deftest test2-rank
  (is (= 10 (rank "TC")))
  (is (= 4 (rank "4D")))
  (is (= 6 (rank "6H")))
  (is (= 13 (rank "KS")))
  (is (= 14 (rank "AS"))))


;;3 pair
(deftest test3-pair?
  (is (= true (pair? pair-hand)))
  (is (= false (pair? three-of-a-kind-hand))))

;;4 three-of-a-kind?
(deftest test4-three-of-a-kind?
  (is (= true (three-of-a-kind? three-of-a-kind-hand)))
  (is (= false (three-of-a-kind? four-of-a-kind-hand))))

;;5 four-of-a-kind?
(deftest test5-four-of-a-kind?
  (is (= true (four-of-a-kind? four-of-a-kind-hand)))
  (is (= false (four-of-a-kind? three-of-a-kind-hand))))

;;6 flush?
(deftest test6-flush?
  (is (= true (flush? flush-hand)))
  (is (= false (flush? straight-flush-hand))))

;;7 full-house?
(deftest test7-full-house?
  (is (= true (full-house? full-house-hand)))
  (is (= false (full-house? two-pairs-hand))))

;;8 two-pairs?
(deftest test8-two-pairs?
  (is (= true (two-pairs? two-pairs-hand)))
  (is (= false (two-pairs? full-house-hand))))

;;9 straight?
(deftest test9-straight?
  (is (= true (straight? low-ace-straight-hand)))
  (is (= false (straight? high-ace-straight-flush-hand))))

;;10 straight-flush?
(deftest test10-straight-flush?
  (is (= true (straight-flush? low-ace-straight-flush-hand)))
  (is (= false (straight-flush? high-ace-straight-hand))))

;;11 value
(deftest test11-value
  (is (= 0 (value high-ace)))
  (is (= 4 (value low-ace-straight-hand)))
  (is (= 7 (value four-of-a-kind-hand)))
  (is (= 8 (value straight-flush-hand)))
  (is (= 6 (value full-house-hand))))

;;12 kickers
(deftest test12-kickers
  (is (= '(14 9 5 4 3) (kickers high-ace)))
  (is (= '(5 4 3 2 1) (kickers low-ace-straight-hand)))
  (is (= '(2 7) (kickers four-of-a-kind-hand)))
  (is (= '(2 7 4) (kickers three-of-a-kind-hand)))
  (is (= '(2 5) (kickers full-house-hand))))

;;13 higher-kicker?
(deftest test13-higher-kicker?
  (is (= true (higher-kicker? '(14 9 7 4 3) '(14 9 5 4 3))))
  (is (= false (higher-kicker? '(2 5) '(2 7)))))

;;14 beats?
(deftest test14-beats?
  (is (= true (beats? high-ace high-seven)))
  (is (= false (beats? low-ace-straight-flush-hand straight-flush-hand))))

;;15 winning-hand
(deftest test15-winning-Hand
  (is (= high-ace (winning-Hand high-ace)))
  (is (= two-pairs-hand (winning-Hand high-ace two-pairs-hand)))
  (is (= low-ace-straight-hand (winning-Hand high-ace low-ace-straight-hand two-pairs-hand)))
  (is (= flush-hand(winning-Hand
                     high-ace low-ace-straight-hand two-pairs-hand flush-hand)))
  (is (= four-of-a-kind-hand (winning-Hand
                               four-of-a-kind-hand high-ace low-ace-straight-hand two-pairs-hand flush-hand))))

(run-tests)
