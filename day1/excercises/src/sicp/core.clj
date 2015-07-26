(ns sicp.core
  (:gen-class))

; Ch 1.2
; 1.11 (fibs!)
; 1.21
; 
; Ch 1.3
; 1.30
; 1.31
; 1.32 } dependent on 1.31
; 1.33 } dependent on 1.32
; 1.34 - note (lambda (x) ...) in Clojure is (fn [x] ...)
; 1.41
; 1.42
; 1.43
;  
; 
; If you are hungry for more, some of the more advanced and demanding ones are:
; Ch 1.2
; 1.16
; 1.17
; 1.18 } these three are combined
; ---
; 1.27
; 1.28 } some cool number theory in these two
; 
; Ch 1.3
; 1.35 - important lambda calculus stuff
; 1.36
; 1.40
; ---
; 1.44
; 1.45
; 1.46 } last three are math heavy and enlightening

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

; 1.9
(defn plusr [a b]  
  (if (= a 0)  
      b  
      (inc (plusr (dec a) b))))  
  
(defn plusi [a b]  
  (if (= a 0)  
      b  
      (plusi (dec a) (inc b))))

; plusr 4 5
; (inc (plusr (dec 4) 5))
; (inc (inc (plusr (dec 3) 5)))
; (inc (inc (inc (plusr (dec 2) 5))))
; (inc (inc (inc (inc (plusr (dec 1) 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

; recursive

; plusi 4 5
; (plusi (dec 4) (inc 5))
; (plusi 3 6)
; (plusi (dec 3) (inc 6))
; (plusi 2 7)
; (plusi (dec 2) (inc 7))
; (plusi 1 8)
; (plusi (dec 1) (inc 8))
; (plusi 0 9)
; 9

; iterative

; 1.10
(defn A [x y]  
  (cond (= y 0) 0  
        (= x 0) (* 2 y)  
        (= y 1) 2  
        :else (A (- x 1)  
                 (A x (- y 1)))))
; (A 1 10)
; 1024

; (A 2 4)
; 65536

; (A 3 3)
; 65536

(defn f [n] (A 0 n)) ; (* 2 n)

(defn g [n] (A 1 n)) ; (A 1 n) ; (A 0 (A 1 (- n 1))); (pow 2 n)

(defn h [n] (A 2 n)) ; (pow 2 (pow 2 n))

(defn k [n] (* 5 n n))

; (A 3 3)
; (A (- 3 1) (A 3 (- 3 1)))
; (A 2 (A 3 2))
; (A 2 (A (- 3 1) (A 3 (- 2 1))))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A (- 2 1) (A 2 (- 2 1))))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A (- 1 1) (A 1 (- 2 1))))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 (* 2 2))
; (A 2 4)
; 65536

(defn count-change [amount]

  (defn first-denom [kinds-of-coins]
    (cond (= kinds-of-coins 1) 1
          (= kinds-of-coins 2) 5
          (= kinds-of-coins 3) 10
          (= kinds-of-coins 4) 25
          (= kinds-of-coins 5) 50))

  (defn cc [amount kinds-of-coins]
    (cond (= amount 0) 1
          (or (< amount 0)
              (= kinds-of-coins 0)) 0
          :else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denom kinds-of-coins))
                       kinds-of-coins))))

  (cc amount 5))

; ex 1.11
(defn fr [n]
  (cond (< n 3) n
        :else (+ (fr (- n 1)) 
                 (* 2 (fr (- n 2))) 
                 (* 3 (fr (- n 3))))))

(defn fi [n]

  (defn calc [n1 n2 n3]
    (+ n1 (* 2 n2) (* 3 n3)))
  
  (defn fi-acc [n1 n2 n3 counter]
    (cond (= counter 3) (calc n1 n2 n3)
          :else (fi-acc (calc n1 n2 n3)
                        n1
                        n2
                        (- counter 1))))

  (cond (< n 3) n
        :else (fi-acc 2 1 0 n)))

(defn gcd [a b]  
  (if (= b 0)  
      a  
      (gcd b (mod a b))))

; 1.21
(defn square [x]
  (java.lang.Math/pow x 2))

(defn divides? [a b]
  (= (mod b a) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

; (smallest-divisor 199)
; 199

; (smallest-divisor 1999)
; 1999

; (smallest-divisor 19999)
; 7

; 1.30
(defn sum [term a next-term b]  
  (defn iter [a result]  
    (if (> a b) 
        result 
        (iter (next-term a) (+ (term a) result))))  
  (iter a 0))

(defn cube [x] (* x x x))

(defn sum-cubes [a b]  
  (sum cube a inc b))
  
(defn sum-integers [a b]  
  (sum identity a inc b))

(defn pi-sum [a b]  
  (defn pi-term [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [x] 
    (+ x 4))
  (sum pi-term a pi-next b))

; 1.31
(defn product [term a next-term b]
  (defn iter [a result]  
    (if (> a b) 
        result 
        (iter (next-term a) (* (term a) result))))  
  (iter a 1))

(defn product' [term a next-term b]
  (if (> a b)
    1
    (* (term a)
       (product' term (next-term a) next-term b))))

(defn factorial [n]
  (product identity 1 inc n))

(defn pi-prod [n]
  (defn pi-prod-term [x]
    (if (divides? 2 x)
        (/ (+ 2 x) (+ 1 x))
        (/ (+ 1 x) (+ 2 x))))
  (product pi-prod-term 1 inc n))

; 1.32
(defn accumulate [combiner null-value term a next-term b]
  (defn iter [a result]
    (if (> a b)
        result
        (iter (next-term a) (combiner (term a) result))))
  (iter a null-value))

(defn sum-acc [term a next-term b]
  (accumulate + 0 term a next-term b))

(defn prod-acc [term a next-term b]
  (accumulate * 1 term a next-term b))

(defn accumulate' [combiner null-value term a next-term b]
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate' combiner null-value term (next-term a) next-term b))))

(defn sum-acc' [term a next-term b]
  (accumulate' + 0 term a next-term b))

(defn prod-acc' [term a next-term b]
  (accumulate' * 1 term a next-term b))

; 1.33
(defn filtered-accumulate [filter? combiner null-value term a next-term b]
  (defn iter [a result]
    (if (> a b)
        result
        (if (filter? a)
            (iter (next-term a) (combiner (term a) result))
            (iter (next-term a) result))))
  (iter a null-value))

(defn accumulate'' [combiner null-value term a next-term b]
  (defn include? [a]
    true)
  (filtered-accumulate include? combiner null-value term a next-term b))

(defn sum-primes-squared [a b]
  (filtered-accumulate prime? + 0 square a inc b))

(defn product-relatively-prime [a b]
  (defn relatively-prime? [x]
    (= (gcd x b) 1))
  (filtered-accumulate relatively-prime? * 1 identity a inc b))

; thought this was supposed to be 12?
; sicp.core=> (def x 2)
; #'sicp.core/x
; sicp.core=> (let [x 3 y (+ x 2)] (* x y))
; 15
; sicp.core=> (let [y (+ x 2) x 3] (* x y))
; 12

; vs

; scheme@(guile-user) [2]> (define x 2)
; scheme@(guile-user) [2]> (let ((x 3) (y (+ x 2))) (* x y))
; $5 = 12

; 1.34
(defn f [g]
  (g 2))

; (f f)
; (f 2)
; (2 2)
; ??

; 1.41
(defn double-it [g]
  (fn [x] (g (g x))))

; sicp.core=> (((double-it (double-it double-it)) inc) 5)
; 21

; 1.42
(defn compose [f g]
  (fn [x] (f (g x))))

; 1.43
(defn repeated [f x]
  (defn iter [a result]
    (if (= a 1)
       (fn [y] (result y))
       (iter (- a 1) (compose f result))))
  (iter x f))
