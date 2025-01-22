(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")

#|
;; WEB Sun Jan 19, 2025

;; interesting -- I may need to go to the non-empty initial env to make this work.
> (run* (q) (evalo '(quote cat) q))
((value cat) (error (unbound-variable quote)))

> (load "q-error.scm")
> (time (run 1 (q) (evalo q q)))
(time (run 1 ...))
    no collections
    0.000163625s elapsed cpu time
    0.000156000s elapsed real time
    37024 bytes allocated
((error (unbound-variable error)))

;; hmm -- looks like an error in the code for sure
> (time (run* (q) (evalo '((lambda (_.0) (list _.0 (list 'quote _.0)))
                            '(lambda (_.0) (list _.0 (list 'quote _.0))))
                          q)))
(time (run* (q) ...))
    no collections
    0.000068334s elapsed cpu time
    0.000069000s elapsed real time
    23984 bytes allocated
((error (unbound-variable quote)))

|#

(define (evalo expr val)
  (eval-expro expr '() val))

(define eval-expro
  (lambda (expr env val)
    (conde
      ((fresh (v)
         (== `(quote ,v) expr)
         (not-in-envo 'quote env)
         (absento 'closure v)
         (== `(value ,v) val)))
      ((fresh (e1 e2 ev1)
         (== `(cons ,e1 ,e2) expr)
         (not-in-envo 'cons env)
         (absento 'closure `(,e1 . ,e2))
         (eval-expro e1 env ev1)
         (conde
           ((fresh (err1)
              (== `(error ,err1) ev1)
              (== `(error ,err1) val)))
           ((fresh (v1 ev2)
              (== `(value ,v1) ev1)
              (eval-expro e2 env ev2)
              (conde
                ((fresh (v2)
                   (== `(value ,v2) ev2)
                   (== `(,v1 . ,v2) val)))
                ((fresh (err2)
                   (== `(error ,err2) ev2)
                   (== `(error ,err2) val)))))))))
      ((symbolo expr) (lookupo expr env val))
      ((fresh (rator rand rator-val)
         (== `(,rator ,rand) expr)
         (eval-expro rator env rator-val)
         (conde
           ((fresh (x body env^ rand-val)
              (== `(value (closure ,x ,body ,env^)) rator-val)
              (eval-expro rand env rand-val)
              (conde
                ((fresh (a body-val)
                   (== `(value ,a) rand-val)
                   (eval-expro body `((,x . ,a) . ,env^) body-val)
                   (conde
                     ((fresh (bv)
                        (== `(value ,bv) body-val)
                        (== `(value ,bv) val)))
                     ((fresh (body-err)
                        (== `(error ,body-err) body-val)
                        (== `(error ,body-err) val))))))
                ((fresh (rand-err)
                   (== `(error ,rand-err) rand-val)
                   (== `(error ,rand-err) val))))))
           ((fresh (rator-err)
              (== `(error ,rator-err) rator-val)
              (== `(error ,rator-err) val))))))
      ((fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (not-in-envo 'lambda env)
         (== `(value (closure ,x ,body ,env)) val))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((== '() env)))))

(define lookupo
  (lambda (x env val)
    (conde
      ((== '() env)
       (== `(error (unbound-variable ,x)) val))
      ((fresh (rest y v)
         (== `((,y . ,v) . ,rest) env)
         (conde
           ((== y x) (== `(value ,v) val))
           ((=/= y x) (lookupo x rest val))))))))
