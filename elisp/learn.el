;;; learn.el --- lerning emacs etc...  -*- lexical-binding: t -*-

;; learning, exemples...  --------------------------------------------------

(comment "misc"

         ;;equivalent
         (defalias 'xp-id (lambda (a) a))
         (defun xp-id (a) a)

         (let ((a 1) (b 2)) (+ a b))

         (let* ((a 1) (b a)) (+ a b))

         (cl-flet ((yop (a) "yop"))
           (yop 42))

         (cl-flet ((f (fn (a b) (+ a b))))
           (f 1 2))

         ;; doesn't work
         (cl-flet ((f1 () (print "a"))
                   (f2 () (call f1) (print "b"))
                   (f3 () (call f2) (print "c")))
           (f3)
           "d")

         ;; have to do this
         (let* ((f1 (fn () (print "a")))
                (f2 (fn () (call f1) (print "b")))
                (f3 (fn () (call f2) (print "c"))))
           (call f3)
           "d")

         (setq my-list '(0 1 2 3 4 5))
         (cl-letf (((car my-list) 1000)
                   ((elt my-list 3) 200))
           (message "%s" my-list)) ;; ==> "(1000 1 2 200 4 5)"

         (intern "aze")
         (symbol-name 'aze)

         (setq vec1 [1 5 3 -2 7 -9 3])

         [a b c] ; quoted like '()

         (setq bar '(1 2)) ;⇒ (1 2)
         (setq x (vector 'foo bar)) ;⇒ [foo (1 2)]
         (setq y (copy-sequence x))

         (elt [1 2 3 4] 2) ;nth

         (destructuring-bind (start . end) (cons 'x 'y) (list start end))

         ;; vars have both value and function value
         (defvar azer 42)
         (fset 'azer (fn () (print "pouet")))
         (print azer (azer))


         ;; error
         (condition-case _
             (forward-sexp)
           (error :scan-error))

         (macroexpand '(defmacro xp-add-m (a b) `(+ ',a ,@b)))
         (defalias
           (quote xp-add-m)
           (cons (quote macro)
                 (function
                  (lambda (a b)
                    (\` (+ (\, a) (\, b)))))))
         
         )

(comment "car, cdr and co"
       (setq list1 '(((x y z) 2 3 4) (a b c) (1 2) o p))
       (car list1)
       (cadr list1)
       (cdar list1)
       (caar list1)
       (cddr list1)
       (cdadr list1)
       (cadaar list1)
       (cddaar list1))

(comment "seqs"
        (seq-elt [1 2 3 4] 2)
        (setq vec [1 2 3 4])(setf (seq-elt vec 2) 5)

        (seqp [1 2])
        (seqp "ert")
        (seqp '(a b c))


        (seq-into [1 2 3] 'list)
        (seq-into "hello" 'vector)

        (seq-drop [1 2 3 4 5 6] 3)
        (seq-take [1 2 3 4 5] 3)

        (seq-subseq '(1 2 3 4 5) 1)
        (seq-subseq '[1 2 3 4 5] 1 3)
        (seq-subseq '[1 2 3 4 5] -3 -1)

        (seq-contains '(symbol1 symbol2) 'symbol1)
        (seq-position '(a b c) 'b)

        (seq-concatenate 'list '(1 2) '(3 4) [5 6]) ;(1 2 3 4 5 6)
        (seq-concatenate 'string "Hello " "world")  ; "Hello world"

        (seq-min [3 1 2])
        (seq-max "Hello")
        (seq-uniq '(1 2 2 1 3))
        (seq-reverse '(1 2 3 4))

        (seq-take-while (lambda (elt) (> elt 0)) '(1 2 3 -1 -2))
        (seq-drop-while (lambda (elt) (> elt 0)) '(1 2 3 -1 -2))

        (seq-map #'1+ '(2 4 6))
        (seq-mapn #'+ '(2 4 6) '(20 40 60))
        (seq-filter (lambda (elt) (> elt 0)) [1 -1 3 -3 5])
        (seq-remove (lambda (elt) (> elt 0)) [1 -1 3 -3 5])
        (seq-reduce #'+ [1 2 3 4] 0)

        (seq-some 'oddp '(1 2 3 4))
        (seq-find 'oddp '(1 2 3 4))

        (seq-every-p #'numberp [2 4 6])
        (seq-empty-p "not empty")

        (seq-count (fn (e) (> e 0)) [-1 2 0 3 -2])
        (seq-sort (fn (a b) (> a b)) [1 5 3 -2 7 -9 3])

        (seq-mapcat #'seq-reverse '((3 2 1) (6 5 4)))
        (seq-partition '(0 1 2 3 4 5 6 7) 3)

        (seq-intersection [2 3 4 5] [1 3 5 6 7])
        (seq-difference '(2 3 4 5) [1 3 5 6 7])

        (seq-group-by #'integerp '(1 2.1 3 2 3.2)) ;⇒ ((t 1 3 2) (nil 2.1 3.2))
        (seq-group-by #'car '((a 1) (b 2) (a 3) (c 4))) ;((b (b 2)) (a (a 1) (a 3)) (c (c 4)))

        (seq-let [first second] [1 2 3 4] (list first second))
                                        ;(1 2)
        (seq-let (_ a _ b) '(1 2 3 4) (list a b))
                                        ;(2 4)
        (seq-let [a [b [c]]] [1 [2 [3]]] (list a b c))
                                        ;(1 2 3)
        (seq-let [a b &rest others] [1 2 3 4] others)
                                        ;[3 4]
        (seq-let [[_ _ _ &rest x]] [[1 2 3 4]] x))

(comment "pcase macro"
         ;https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-matching-case-statement.html#Pattern-matching-case-statement
         )

(comment "maps"

       (defun hnew ()
         (make-hash-table :test 'equal))

       (defun hput  (h k v &rest kvs)
         (if kvs
             (progn  (puthash k v h) (apply 'hput h kvs) h)
           (progn (puthash k v h) h)))

       (defun hget  (h k)
         (cond
          ((not k) h)
          ((listp k)
           (let ((found (hget h (car k))))
             (cond
              ((not (cdr k)) found)
              ((hm? found) (hget found (cdr k))))))
          ('else (gethash k h))))

       

       (defun hm  (&rest xs)
         (apply 'hput (hnew) xs))

       (defun hm*  (&rest args)
         (apply 'hm (apply 'list* args)))

       (defun hm? (h)
         (equal 'hash-table (type-of h)))

       (defun hrem (h k &rest ks)
         (if ks
             (apply 'hrem (hrem h k) ks)
           (progn (remhash k h) h)))

       (defun hmrg (h &rest hs)
         ())

       ;; exemples ---

       (progn
         (hput (hnew) :a 1 :b 2)
         (hget (hm :a 1 :b (hm :c 1)) :a)
         (hget (hm :a 1 :b (hm :c 1)) '(:b :c))
         (hrem (hm :a 1 :b 2 :c 3) :a :c))
 )

(comment "assoc list" 
         (setq x
               '(("mary" . 23)
                 ("john" . 24)
                 ("smith" . 33)))

         (assoc "john" x)
         (cdr (assoc "john" x))
         (rassoc 24 x))
