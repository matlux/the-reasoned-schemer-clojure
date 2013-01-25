(ns reasoned-schemer-clj.core
  (:refer-clojure :exclude [== >= <= > < =])
  (:use clojure.core.logic
        [clojure.core.logic.arithmetic :only [>= <= > < =]])
)


(run* [q]
      (== q true))

(run* (q)
      u#
      (== 'corn q))

(run* (q)
      
      (let [x false]
        (== false q)))

(run* (q)
      (fresh [x]
             (== false x)
             (== true x)
             (== true q)))
(run* (q)
      s#)

(defn pair? [x]
   (or (lcons? x) (and (coll? x) (seq x))))

(run* (r)
      (fresh [x]
             (let [y x]
               (fresh [x]
                      (== (lcons y (lcons x (lcons y '()))) r)))))


(run* (r)
      (fresh [x y]
             (== (lcons x (lcons y '())) r)))
(run* (q)
      (fresh [x]
             (== true x)
             (== x q)))

(run* (q)
      (conde
       [u# s#]
       [s# u#]))

(run* (q)
      (conde
       [s# s#]
       [s# u#]))

(run* (q)
      (conde
       [(== 'olive q) s#]
       [(== 'oil q) s#]))

(run* (q)
      (conde
       [(== 'virgin q) u#]
       [(== 'olive q) s#]
       [s# s#]
       [(== 'oil q) s#]))

(run* (q)
      (fresh [x y]
             (conde
              [(== 'split x) (== 'pea y)]
              [(== 'navy x) (== 'bean y)])
             (== (lcons x (cons y '())) q)))

(run* (q)
      (fresh [x y]
             (conde
              [(== 'split x) (== 'pea y)]
              [(== 'navy x) (== 'bean y)])
             (== (list x y) q)))

(run* [q]
  (fresh [x y z]
    (conde
      ((== x "1st"))
      ((== y "2nd"))
      ((== z "3rd")))
    (== q [x y z])))

;;(doc conde)

(def teacupo (fn [x]
               (conde
                [(== 'tea x) s#]
                [(== 'cup x) s#])))

(run* (x) (teacupo x))

(run* (r)
      (fresh [x y]
             (conde
              ((teacupo x) (== true y) s#)
              ((== false x) (== true y)))
             (== (lcons x (lcons y '())) r)))

(run* (r)
      (fresh (x y z)
             (conde
              ((== y x) (fresh (x) (== z x)))
              ((fresh (x) (== y x)) (== z x)))
             (== (lcons y (lcons z '())) r)))

(run* (r)
      (fresh (x y z)
             (conde
              ((== y x) (fresh (x) (== z x)))
              ((fresh (x) (== y x)) (== z x)))
             (== false x)
             (== (lcons y (lcons z '())) r)))

(run* (q)
      (let [a (== true q)
            b (fresh [x]
                     (== x q)
                     (== false x))
            c (conde
               [(== true q) s#])]
        b))

(run* (r)
      (fresh (x y)
             (== (list x y) r)))


(run* (r)
      (firsto '(a c o r n) r))

(run* (r)
      (fresh (x y)
             (firsto (list r y) x)
             (== 'pear x)))


(run* (r)
      (fresh (x y)
             (resto '(grape raisin pear) x)
             (firsto '((a) (b) (c)) y)
             (== (lcons x y) r)))

;(use 'reasoned-schemer-clj)
;;(doc project)