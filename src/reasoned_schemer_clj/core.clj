(ns reasoned-schemer-clj.core
  (:refer-clojure :exclude [== >= <= > < =])
  (:use clojure.core.logic
        [clojure.core.logic.arithmetic :only [>= <= > < =]])
)

;; Chapter 1: Playthings

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
;; Chapter 2: Teaching Old Toys New Tricks

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

(run* (l)
    (fresh (d x y w s)
      (conso w '(a n s) s) 
      (resto l s)
      (firsto l x)
      (== 'b x)
      (resto l d)
      (firsto d y)
      (== 'e y)))
(empty? '(grape rain pear))

(empty? '())

(run* (q)
      (emptyo '(grape raisin pear))
      (== true q))

(run* (q)
      (emptyo '())
      (== true q))

(run* (x)
      (emptyo x))

(def nullo
  (fn [x]
    (== x '())))

(run* (x)
      (nullo x))

;(source emptyo)

(clojure.core/= 'pear 'plum)
(clojure.core/= 'plum 'plum)

(run* (q)
      (== 'pear 'plum)
      (== true q))

(def eqo (fn [x y]
       (== x y)))

(run* (q)
      (eqo 'pear 'plum)
      (== true q))


(defn pair? [x]
  (or (lcons? x) (and (coll? x) (seq x))))


(pair? (llist '(split) 'pea))

; this works
(pair? (lcons '(split) 'pea))

;;this does not really work
(pair? '())
(pair? (llist 'pear nil))


(first '(pear))

(rest '(pear))

(lcons '(split) 'pea)

(def pairo (fn [p]
             (fresh (a d)
                    (conso a d p))))

(run* (q)
      (pairo (lcons q q))
      (== true q))

(run* (q)
      (pairo '())
      (== true q))

(run* (q)
      (pairo 'pair)
      (== true q))

(run* (r)
      (pairo r))

(run* (r)
      (pairo (lcons r 'pear)))


;; Chapter 3: Seeing Old Friends in New Ways
(seq? '((a) (a b) c))

(seq? '())

(seq? 's)

(seq? (list 'd 'a 't 'e 's))

(defn listo [l]
  (conde
    [(emptyo l) s#]
    [(pairo l)
     (fresh [d]
       (resto l d)
       (listo d))]))

(fresh (d)
       (resto 'l d)
       (listo d))

(run* (x)
      (listo (list 'a 'b x 'd)))

(run 1 (x)
      (listo (llist 'a 'b 'c x)))

; don't run this one it's an infinite loop
;(run* (x)
;     (listo (llist 'a 'b 'c x)))



(run 5 (x)
     (listo (llist 'a 'b 'c x)))

;; this returns no results. It does not work like in the book. You have to use the list instead (see above) to be consistent with the book.
;; It seems llist should be used when a fresh variable is on the right most side of the listo. Otherwise list should be used ?
(run 5 (x)
     (listo (llist 'a 'b 'c x 'd)))

(defn lol? [l]
  (cond
   (empty? l) true
   (seq? (first l)) (lol? (rest l))
   :else false))


(lol? '((1 2 3) (4 5 6)))
;true

(defn lolo [l]
  (conde
   [(emptyo l) s#]
   [(fresh (a)
           (firsto l a)
           (listo a))
    (fresh (b)
           (resto l b)
           (lolo b))]))

(run 1 (l)
     (lolo l))

(run 1 (q)
     (fresh (x y)
            (lolo '(('a 'b) (x 'c) ('d y)))
            (== true q)))


(run 1 (q)
     (fresh (x)
            (lolo (list (list 'a 'b) x))
            (== true q)))

(defn twino [s]
  (fresh (x y)
         (conso x y s)
         (conso x '() y)))
(defn twino2 [s]
  (fresh (x)
         (conso x (list x) s)))

(defn twino3 [s]
  (fresh (x)
         (== (list x x) s)))

(run* (q)
      (twino '(tofu tofu))
      (== true q))

(run* (z)
      (twino3 (list z 'tofu)))

(defn loto [l]
  (conde
   [(emptyo l) s#]
   [(fresh (a)
           (firsto l a)
           (twino a))
    (fresh (d)
           (resto l d)
           (loto d))]))

(run 1 (z)
     (loto (llist (list 'g 'g) z)))

(run 5 (r)
     (fresh (w x y z)
            (loto (llist (list 'g 'g) (list 'e w) (list x y) z))
            (== (list w (list x y) z) r)))

(defn listofo [predo l]
  (conde
   [(emptyo l) s#]
   [(fresh (a)
           (firsto l a)
           (predo a))
    (fresh (d)
           (resto l d)
           (listofo predo d))]))

(run 3 (out)
     (fresh (w x y z)
            (== (llist (list 'g 'g) (list 'e w) (list x y) z) out)
            (listofo twino out)))

(defn loto2 [l]
  (listofo twino l))

(defn eq-caro [l x]
  (firsto l x))


(defn membero2 [x l]
  (conde
   [(eq-caro l x) s#]
   [(fresh (d)
           (resto l d)
           (membero2 x d))]))

(run* [q]
      (membero 'olive (list 'virgin 'olive 'oil))
      (== true q))

(run 1 [y]
     (membero y (list 'hummus 'with 'pita)))
(run* [y]
     (membero2 y (list 'hummus 'with 'pita)))

(defn identityo [l]
  (run* [y]
        (membero y l)))

(run* [x]
      (membero 'e (list 'pasta x 'fagioli)))

(run 1 [x]
     (membero 'e (list 'pasta 'e x 'fagioli)))
(run 1 [x]
      (membero 'e (list 'pasta x 'e 'fagioli)))

(run* [r]
      (fresh (x y)
             (membero 'e (list 'pasta x 'fagioli y))
             (== (list x y) r)))

(run 5 [l]
      (membero2 'tofu l))

(defn pmembero [x l]
  (conde
   [(emptyo l) u#]
   [(eq-caro l x) (resto l '())]
   [(fresh (d)
           (resto l d)
           (pmembero x d))]))

(run 5 [l]
      (pmembero 'tofu l))

;this is not working like in the book: Only getting one true i.e. (true) instead of (true true)
(run 5 [q]
     (pmembero 'tofu (list 'a 'b 'tofu 'd 'tofu))
     (== true q))

(defn pmembero2 [x l]
  (conde
   [(emptyo l) u#]
   [(eq-caro l x) (resto l '())]
   [(eq-caro l x) s#]
   [(fresh (d)
           (resto l d)
           (pmembero x d))]))

;; this is not working like in the book: Only getting one true i.e. (true) instead of (true true). Not sure why?
(run 5 [q]
     (pmembero2 'tofu (list 'a 'b 'tofu 'd 'tofu))
     (== true q))

(run 5 [l]
     (pmembero2 'tofu l))

(defn pmembero3 [x l]
  (conde
   [(emptyo l) u#]
   [(eq-caro l x) (resto l '())]
   [(eq-caro l x) (fresh (a d)
                         (resto l (llist a d)))]
   [(fresh (d)
           (resto l d)
           (pmembero x d))]))

;; not exactly like the book. Could it be for lack of "else" clause in the conde?
(run 15 [l]
      (pmembero3 'tofu l))

(defn first-value [l]
  (run 1 [y]
       (membero y l)))

(first-value (list 'pasta 'e 'fagioli))

(defn memberrevo [x l]
  (conde
   [(emptyo l) u#]
   [s# (fresh (d)
              (resto l d)
              (memberrevo x d))]
   [(eq-caro l x)]))

;; not working as expected. Is is because of lack of else clause again? Can anyone tell me how to simulate the else clause?
(run* [x]
      (memberrevo x (list 'pasta 'e 'fagioli)))

(defn reverse-list [l]
  (run* (y)
        (memberrevo y l)))

;; not working
(reverse-list (list 'a 'b 'c))

;; Chapter 4 : Members only



(defn memo [x l out]
  (conde
    [(emptyo l) u#]
    [(firsto l x) (== l out)]
    [(fresh (d)
            (resto l d)
            (memo x d out))])
  )



(defn rembero [x l out]
  (conde
   [(emptyo l) (== '() out)]
   [(firsto l x) (resto l out)]
   [(fresh (res)
           (fresh (d)
                  (resto l d)
                  (rembero x d res))
           (fresh (a)
                  (firsto l a)
                  (conso a res out)))]))

(run 1 [out]
     (fresh (y)
            (rembero 'peas (list 'a 'b y 'd 'peas 'e) out)))

(run 1 [out]
     (memo 'tofu (list 'a 'b 'tofu 'd 'tofu 'e) out))
;(source membero)

(use 'reasoned-schemer-clj.core :reload)
                                        ;(doc clojure.core.logic/distinctfd)

;(source seq?)