(def high-seven ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand ["2H" "4S" "4C" "5C" "7D"])
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





;;  (suit card) ја враќа бојата на картата, една од "C", "D", "H", "S".
 (defn suit
   "returns the suit of `card`

   implementation:
   returns the second char of `card` as string"
   [card] (str(second card)))


;;  . (rank card) ја враќа вредноста на картата, како цел број во опсег
;; помеѓу 2 и 14 (hint: Искористете мапа со која ќе ја дефинирате
;; конверзијата на карактерите T, J, Q, K, A во броевите од 10 до 14.
;; Употребете ги Java методите (Character/isDigit char) и
;; (Integer/valueOf string).)
(defn rank
  "returns the rank of `card`"
  [card]
  (cond
    (Character/isDigit (first card)) (Integer/valueOf (str(first card)))
    (= \T (first card)) 10
    (= \J (first card)) 11
    (= \Q (first card)) 12
    (= \K (first card)) 13
    (= \A (first card)) 14
    :else nil))


;; (pair? hand) враќа true ако комбинацијата која се проследува на влез
;; содржи две (но не повеќе од две) карти со иста вредност.
(defn pair?
  "returns true if `hand` contains a single pair

  implementation:
  sorts the frequencies of the list of ranks of the cards in `hand` if it
  matches (1 1 1 2) then `hand` contains a single pair"
  [hand]
  (= '(1 1 1 2)(sort(vals (frequencies (map rank hand))))))

;; . (three-of-a-kind? hand) враќа true ако комбинацијата која се
;; проследува на влез содржи три (но не повеќе) карти со иста вредност.
(defn three-of-a-kind?
  "returns true if `hand` contains a three-of-a-kind

  implementation:
  sorts the frequencies of the list of ranks of the cards in `hand`, if it
  matches (1 1 3) then `hand` contains three-of-a-kind"
  [hand]
  (= '(1 1 3)(sort(vals (frequencies (map rank hand))))))

;; (four-of-a-kind? hand) враќа true ако комбинацијата која се
;; проследува на влез содржи четири карти со иста вредност.
(defn four-of-a-kind?
    "returns true if `hand` contains a four-of-a-kind

  implementation:
  sorts the frequencies of the list of ranks of the cards in `hand`, if it
  matches (1 4) then `hand` contains four-of-a-kind"
  [hand]
  (= '(1 4)(sort(vals (frequencies (map rank hand))))))

;; (flush? hand) враќа true ако сите карти во комбинацијата која се
;; проследува на влез се со иста боја, но не се во секвенца (последователни).
(defn same-suit?
  "returns true if all cards in `hand` are in same suit

  implementation:
  return ture if the frequencies of the list of suits of the cards in `hand` has value 5"
  [hand]
  (= '(5)(vals (frequencies (map suit hand)))))

(defn in-seq?
  "return tue if all cards in `hand` are in sequence

  implementation:
  use `reduce` on the sorted list of ranks of the cards in `hand`
  with reducing function that returns the second argument if it is +1 of the first argument
  else returns nil

  special case `hand` contains low-ace-straight"
  [hand]
  (or (if (reduce #(if (= %1 (dec %2)) %2) (sort(map rank hand))) true)
      (= '(2 3 4 5 14)(sort(map rank hand)))))

(defn flush?
  "return true if all cards in `hand` are in same suit but not in sequence"
  [hand] (and (same-suit? hand) (not (in-seq? hand))))

;; (full-house? hand) враќа true ако комбинацијата која се проследува
;; на влез содржи три карти со иста вредност и уште две карти со иста
;; вредност (комбинација 3+2).
(defn full-house?
  "returns true if `hand` contains a full-house ie. a three-of-a-kind and a pair

  implementation:
  sorts the frequencies of the list of ranks of the cards in `hand`, if it
  matches (2 3) then `hand` contains full-house"
  [hand]
  (= '(2 3)(sort(vals (frequencies (map rank hand))))))

;; (two-pairs? hand) враќа true ако комбинацијата која се проследува на
;; влез содржи два парови на карти кои имаат иста вредност (комбинација
;; 2+2, но не и комбинација 3+2).
(defn two-pairs?
  "returns true if `hand` contains two pairs

  implementation:
  sorts the frequencies of the list of ranks of the cards in `hand`, if it
  matches (1 2 2) then `hand` contains two pairs"
  [hand]
  (= '(1 2 2)(sort(vals (frequencies (map rank hand))))))

;; (straight? hand) враќа true ако комбинацијата која се проследува на
;; влез содржи карти кои имаат 5 последователни вредности (на
;; пример, ["2H" "3S" "6C" "5D" "4D"]). Асот може да се смета или како
;; вредност 14 или како вредност 1.
(defn straight?
  "return true if all cards in `hand` are in sequence but not in same suit"
  [hand] (and (in-seq? hand) (not (same-suit? hand))))

;; (straight-flush? hand) враќа true ако комбинацијата која се
;; проследува на влез истовремено е и straight и flush.
(defn straight-flush?
  "return true if all cards in `hand` are in sequence and in same suit"
  [hand] (and (same-suit? hand) (in-seq? hand)))

;; (value hand) враќа вредност на комбинација во зависност од тоа
;; за која од претходните функции враќа true:
(defn value
  "maps `hand` into iteger, based on the poker combination it contains"
  [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))

;; (kickers hand) враќа подредена листа од вредности (цели броеви)
;; на картите во комбинацијата на влез, при што подредувањето е
;; дефинирано во зависност од вредноста на самата комбинација:
(defn kickers
  "maps the `hand` in ordered list of integers based on
  the poker combination it contains and the ranks of the cards

  implementation:
  uses build-in function `sort-by` to sort he map of frequencies of
  the list of ranks of the cards in `hand` by comparing the values then the keys
  then reverses the result so the list is in descending order

  special case for low-ace-straight-hand"
  [hand]
  (if (= '(2 3 4 5 14)(sort(map rank hand))) '(5 4 3 2 1)
    (map first
         (reverse(sort-by #(vector (second %)(first %))
                          (frequencies (map rank hand)))))))

;; (higher-kicker? kicker1 kicker2)
(defn higher-kicker?
  "returns true if `kicker1` is higher than `kicker2`

  implementation:
  uses the build-in `compare` fuction just first convertes the arguments in vectors
  becouse `compare` works with vectors not lists"
  [kicker1 kicker2]
  (if (= 1 (compare (vec kicker1) (vec kicker2))) true false))


;; (beats? hand1 hand2)
(defn beats?
  "return true if `hand1` beats `hand2`
  ie. has a higher poker combination value or in case of equal values higher kicker"
  [hand1 hand2]
  (cond
    (> (value hand1)(value hand2)) true
    (< (value hand1)(value hand2)) false
    :else (higher-kicker? (kickers hand1)(kickers hand2))))

;; (winning-hand & hands)
(defn winning-hand
  "returns the wining hand
  ie. the hand with hiest pocker combination value and highest kicker

  implementation:
  uses `reduce` on the arguments list with reducing function
  that returns the argument that beats the other argument"
  [& hands] (reduce #(if (beats? %1 %2) %1 %2) hands))
