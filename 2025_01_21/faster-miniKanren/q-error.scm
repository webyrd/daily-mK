(load "mk-vicare.scm")
(load "mk.scm")
(load "test-check.scm")

#|
WEB Mon Jan 20, 2025

use non-empty initial env and handling of primitives to avoid
generating errors due to unbound variable reference for built-in
special forms

handling of prims, and non-empty initial env, adapted from:

Barliman/cocoa/Barliman/mk-and-rel-interp/interp-simple.scm



Hmm--our situation has not improved...

> (load "q-error.scm")
> (run* (q) (evalo '(quote cat) q))
((value cat)
  (error (unbound-variable quote))
  (error (unbound-variable quote)))

|#

(define empty-env '())

(define initial-env
  `((cons . (primitive . cons))
    . ,empty-env))

(define (evalo expr val)
  (eval-expro expr initial-env val))

(define (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d av)
       (== `(,a . ,d) expr)
       (eval-expro a env av)
       (conde
         ((fresh (erra)
            (== `(error ,erra) av)
            (== `(error ,erra) val)))
         ((fresh (v-a dv)
            (== `(value ,v-a) av)
            (eval-listo d env dv)
            (conde
              ((fresh (errd)
                 (== `(error ,errd) dv)
                 (== `(error ,errd) val)))
              ((fresh (v-d)
                 (== `(value ,v-d) dv)
                 (== `(,v-a . ,v-d) val)))))))))))

(define (eval-primo prim-id a* val)
  (conde
    ((== 'cons prim-id)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val)
       (=/= closure-tag a)
       (=/= 'primitive a)))))

(define eval-expro
  (lambda (expr env val)
    (conde
      ((fresh (v)
         (== `(quote ,v) expr)
         (not-in-envo 'quote env)
         (absento 'closure v)
         (== `(value ,v) val)))
      ((fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (not-in-envo 'lambda env)
         (== `(value (closure ,x ,body ,env)) val)))      
      ((symbolo expr) (lookupo expr env val))
      ((fresh (rator rands ratorv)
         (== `(,rator . ,rands) expr)
         ;; any way to get rid of this without getting extraneous
         ;; (error (unbound-variable quote)) results?
         (conde
           ((=/= 'lambda rator)
            (=/= 'quote rator))
           ((== 'quote rator)
            (fresh (_)
              (lookupo rator env `(value ,_))))
           ((== 'lambda rator)
            (fresh (_)
              (lookupo rator env `(value ,_)))))      
         (eval-expro rator env ratorv)
         (conde
           ((fresh (err-rator)
              (== `(error ,err-rator) ratorv)
              (== `(error ,err-rator) val)))
           ((fresh (prim-id randsv)
              (== `(value (primitive . ,prim-id)) ratorv)
              (eval-listo rands env randsv)
              (conde
                ((fresh (err-rands)
                   (== `(error ,err-rands) randsv)
                   (== `(error ,err-rands) val)))
                ((fresh (a* primv)
                   (== `(value ,a*) randsv)
                   (eval-primo prim-id a* primv)
                   (conde
                     ((fresh (err-primo)
                        (== `(error ,err-primo) primv)
                        (== `(error ,err-primo) val)))
                     ((== `(value ,val) primv)))))))))))
      ((fresh (rator rand rator-val)
         (== `(,rator ,rand) expr)
         ;; any way to get rid of this without getting extraneous
         ;; (error (unbound-variable quote)) results?         
         (conde
           ((=/= 'lambda rator)
            (=/= 'quote rator))
           ((== 'quote rator)
            (fresh (_)
              (lookupo rator env `(value ,_))))
           ((== 'lambda rator)
            (fresh (_)
              (lookupo rator env `(value ,_)))))
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
              (== `(error ,rator-err) val)))))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((== empty-env env)))))

(define lookupo
  (lambda (x env val)
    (conde
      ((== empty-env env)
       (== `(error (unbound-variable ,x)) val))
      ((fresh (rest y v)
         (== `((,y . ,v) . ,rest) env)
         (conde
           ((== y x) (== `(value ,v) val))
           ((=/= y x) (lookupo x rest val))))))))
