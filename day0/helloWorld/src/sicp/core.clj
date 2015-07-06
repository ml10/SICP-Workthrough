(ns sicp.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

; 1.2
(def nested (/
              (+ 5 4
                (- 2
                  (- 3
                    (+ 6
                      (/ 4 5)))))
              (* 3
                (- 6 2)
                (- 2 7))))

; 1.3
(defn square [x] (* x x))

(defn sum-of-squares [x y] (+ (square x) (square y)))

(defn sum-of-2-largest-squares [x y z]
  (cond (and (not (> x y)) (not (> x z))) (sum-of-squares y z)
        (and (not (> y x)) (not (> y z))) (sum-of-squares x z)
        :else (sum-of-squares x y)))

(defn sum-of-largest-2-squares [x y z]
  (let [two-largest (rest (sort [x y z]))]
    (apply sum-of-squares two-largest)))

; 1.4
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b)) ; if evaluates to + or - oprerattion
                          ; based on the sign of b
                          ; if b is pos, do (+ a b)
                          ; if b is neg, do (- a b)
                          ; == (- a (- b))
                          ; == (+ a (abs b))

; 1.5
;(defn p [] (p))
;
;(defn tst [x y]
;  (if (= x 0)
;      0
;      y))
;
;(tst 0 (p)) ; with applicative order: stack overflow (infinite loop)
;            ; with normal order: 0

; 1.6
(defn new-if [predicate then-clause else-clause]
  (cond (predicate) then-clause
        :else else-clause))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough-bad-tolerance? [guess x]
  (< (java.lang.Math/abs (- (square guess) x)) 0.001))

(defn good-enough? [guess x]
  (<
    (java.lang.Math/abs
      (-
        (square guess)
        x))
    (* guess 0.001)))

(defn better-enough? [guess x]
  (<=
    (java.lang.Math/abs
      (-
        (square guess)
        x))
    (java.lang.Math/ulp (double x))))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;(defn sqrt-iter [guess x]
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x)
;                     x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

;; new-if keeps going down the stack trying to find an answer
;; that's good-enough, due to the applicative order
;; so lots more blown stacks
;; also means regular if must be normal order
;; can do this in scala by passing the argument by name

; 1.7
;; for too small numbers the tolerance is too big
;; so the computed root will be off by too much
;; for too large number the tolerance is too low
;; so we won't compute a result, we'll blow stack instead
;; may also run into precision issues for really large numbers

;; see good-enough? above, good-enough-bad-tolerance? is original

;; better-enough? uses ulp from java.lang.Math to get even better
;; it's got stack troubles, though...

(defn show-error [x]
  (- (square (sqrt x)) x))

; 1.8
;; todo make the good-enough and iterators generic enough to reuse
;; nvm... see section 1.1.8... do that instead
(defn improve-cube [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(defn cube [x]
  (java.lang.Math/pow x 3))

(defn good-enough-for-cube? [guess x]
  (<
    (java.lang.Math/abs
      (-
        (cube guess)
        x))
    (* guess 0.001)))

(defn cube-root-iter [guess x]
  (if (good-enough-for-cube? guess x)
      guess
      (cube-root-iter (improve-cube guess x)
                      x)))

(defn cube-root [x]
  (cube-root-iter 1.0 x))

