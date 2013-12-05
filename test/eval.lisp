(defun caar (a) (car (car a)))
(defun cadr (a) (car (cdr a)))
(defun cdar (a) (cdr (car a)))
(defun caddr (a) (cadr (cdr a)))
(defun cadar (a) (car (cdar a)))
(defun caddar (a) (caddr (car a)))
(defun null. (x) (eq x '()))
(defun append. (x y)
    (cond ((null. x) y)
          (true      (cons (car x) (append. (cdr x) y)))))
(defun list (x y) (cons x (cons y '())))
(defun pair. (x y)
    (cond ((null. x) '())
          (true      (cons (list (car x) (car y)) (pair. (cdr x) (cdr y))))))
(defun assoc. (x y)
    (cond ((eq x (caar y)) (cadar y))
          (true            (assoc. x (cdr y)))))
(defun evcon. (c a)
    (cond ((eval. (caar c) a) (eval. (cadar c) a))
          (true               (evcon. (cdr c) a))))
(defun evlis. (m a)
    (cond ((null. m) '())
          (true      (cons (eval. (car m) a) (evlis. (cdr m) a)))))
(defun eval. (e a)
    (cond
        ((atom e) 
            (cond 
                ((eq e 'true) true)
                ((eq e 'false) false)
                (true (assoc. e a))))
        ((atom (car e))
            (cond
                ((eq (car e) 'quote) (cadr e))
                ((eq (car e) 'atom)  (atom  (eval. (cadr e) a)))
                ((eq (car e) 'eq)    (eq (eval. (cadr e) a) (eval. (caddr e) a)))
                ((eq (car e) 'car)   (car  (eval. (cadr e) a)))
                ((eq (car e) 'cdr)   (cdr  (eval. (cadr e) a)))
                ((eq (car e) 'cons)  (cons  (eval. (cadr e) a) (eval. (caddr e) a)))
                ((eq (car e) 'cond)  (evcon. (cdr e) a))
                (true                (eval. (cons (assoc. (car e) a) (cdr e)) a))))
        ((eq (caar e) 'label)
            (eval. (cons (caddar e) (cdr e))
                   (cons (list (cadar e) (car e)) a)))
        ((eq (caar e) 'lambda)
            (eval. (caddar e)
                   (append. (pair. (cadar e) (evlis. (cdr e) a))
                             a)))))
; Tests
(null. 'a)
(null. '())
(append. '(a b) '(c d))
(append. '() '(c d))
(pair. '(x y z) '(a b c))
(assoc. 'x '((x a) (y b)))
(assoc. 'x '((x new) (x a) (y b)))
(eval. 'x '((x a) (y b)))
(eval. '(eq 'a 'a) '())
(append. '() '())
(eval. '(cons x '(b c))
       '((x a) (y b)))
(evcon. '((true 'atom)
          (true 'list)) '())
(eval. '(cond ((atom x) 'atom)
              (true 'list)) '((x '(a b))))
(eval. '(f '(b c))
       '((f (lambda (x) (cons 'a x)))))
(eval. '((label firstatom (lambda (x)
                            (cond ((atom x) x)
                                  (true (firstatom (car x))))))
          y)
       '((y ((a b) (c d)))))

(eval. '((lambda (x y) (cons x (cdr y)))
         'a
         '(b c d))
       '())
((lambda (n) (car n)) '(x x))
((label f (lambda (n)
                    (cond  ((atom n) '(n))
                           ((atom (cdr n)) '(n))
                           (true       (append. (f (cdr n)) (f (cdr (cdr n)))))
                    )
                  )) '(x x x x x x))
(eval. '((label f (lambda (n)
                    (cond  ((atom n) '(n))
                           ((atom (cdr n)) '(n))
                           (true       (append. (f (cdr n)) (f (cdr (cdr n)))))
                    )
                  )) '(x x x x x x)) '(
(null. (lambda (x) (eq x '())))
(append. (label append. (lambda (x y)
    (cond ((null. x)    y)
          (true           (cons (car x) (append. (cdr x) y)))))))
))
; (+ 1 2 3 4 5)
; (eq (+ 1 2) 3)
; (defun f (n) (cond ((eq n 0) 1)
;                    ((eq n 1) 1)
;                    (true (+ (f (- n 1)) (f (- n 2))))))
; (f 6)

