;;; lists.el --- list examples and tools using dash and other  -*- lexical-binding:t -*-
;;;
;;; Examples of using 'dash'

(require 'dash)

(-cut list 1 <> 3 <> 5)
(-map
 (-lambda ((x y))
   (+ x y))
 '((1 2) (3 4) (5 6))) ;; => (3 7 11)

(-map
 (-lambda ([x y])
   (+ x y))
 '([1 2] [3 4] [5 6])) ;; => (3 7 11)

(funcall
 (-lambda ((_ . a) (_ . b))
   (-concat a b))
 '(1 2 3) '(4 5 6)) ;; => (2 3 5 6)

(setq functionfun (lambda (arg) (lambda () (+ 2 arg))))

(funcall (-iteratefn #'functionfun 3) 1)

(-if-let (match-index (string-match "d" "abc")) (+ match-index 3) 7)
