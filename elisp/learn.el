;;; learn.el --- lerning emacs etc...  -*- lexical-binding: t -*-

;; learning, exemples...  --------------------------------------------------

(comment "misc"

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

;; utils ------------------------------------------------------------------

(progn "basic"

       (defun pb-id (&optional a &rest _) a)

       (defun pb-kw-name (x)
         (substring (symbol-name x) 1))

       (defun pb-kw? (x)
         (equal ":" (substring (symbol-name x) 0 1)))

       (defun pb-kw->sym (x)
         (intern (pb-kw-name x)))

       (defun pb-ensure-list (x)
         (if (listp x) x (list x)))

       (defmacro pb-let (bs &rest body)
         `(let* ,(-partition 2 bs) ,@body))

       (defmacro pb-cond (&rest xs)
         `(cond ,(-partition 2 (mapcar 'pb-ensure-list xs))))

       (progn "tests"
              (pb-kw-name :sdf) ;-> "sdf"
              (macroexpand '(pb-let (a b c d) e f g))
              (macroexpand '(pb-cond a b c d (e f g) (h i j) k (l m o) (p q r) s))))

(progn "plists"

       (defun pl (&rest xs)
         (apply 'plput '() xs))

       (defun plget (pl k)
         (cond
          ((not k) pl)
          ((listp k) (plget (plget pl (car k)) (cdr k))) 
          ('else (plist-get pl k))))

       (defun plput-h (pl k v)
         (cond
          ((not k) v)
          ((listp k)
           (plput-h pl
                    (car k)
                    (plput-h (plget pl (car k)) (cdr k) v)))

          ('else
           (plist-put pl k v))))

       (defun plput (pl k v &rest kvs)
         (if kvs
             (apply' plput (plput-h pl k v) kvs)
           (plput-h pl k v)))

       (defun plupd (pl k f &rest kvs)
         (let ((nxt (plput pl k (funcall f (plget pl k)))))
           (if kvs
               (apply 'plupd nxt kvs)
             nxt)))

       (defun plmrg (pl &rest pls)
         (if pls
             (apply 'plmrg (apply 'plput pl (car pls)) (cdr pls))
           pl))

       (progn "tests"

              (pl '(:a :b) 1 :p 2)

              (plget (pl :a 1 :b 2 :c (pl :d 42))  '(:c :d))

              (plput (pl :a 1 :b 2)
                     '(:f :g) 42
                     :a 38)

              (plupd (pl :a 1 :b (pl :c 41))
                     :a '1-
                     '(:b :c) '1+)

              (plmrg (pl :a 1 :b 2)
                     (pl :a 3 :n 0)
                     (pl :p 33))))

;; nav -------------------------------------------------------------------

(defvar opening-delimiters
  '("(" "[" "{"))
(defvar closing-delimiters
  '(")" "]" "}"))
(defvar delimiters
  (apply 'append opening-delimiters closing-delimiters))

(progn "macros"

       (defmacro nv-> (&rest xs)
         (cons 'progn (mapcar 'pb-ensure-list xs)))

       (defmacro nv-?> (p &rest xs)
         `(if ,(pb-ensure-list p) (nv-> ,@xs)))

       (defmacro nv-< (&rest xs)
         (cons 'cond (-partition 2 (mapcar 'pb-ensure-list xs))))

       (defmacro nv-freeze (&rest xs)
         `(let ((nv-freeze-saved nv-sel)
                (nv-freeze-ret (progn (nv-> ,@xs) nv-sel)))
            (setq nv-sel nv-freeze-saved)
            nv-freeze-ret))

       (defmacro nv-if (p a b)
         `(if ,(pb-ensure-list p)
             ,(pb-ensure-list a)
           ,(pb-ensure-list b)))

       (progn "tests" 
         (macroexpand '(nv-> a b c))
         (macroexpand '(nv-?> a b c))
         (macroexpand '(nv-< a b c d (f g) h o (p q r)))
         (macroexpand '(nv-freeze nv-last nv-prev))
         (macroexpand '(nv-if (a b) (c b) c))
         (macroexpand '(nv-if a b (e r c)))
         (macroexpand '(nv-if a (b) c)))

       )

(defun nv-sel (x y)
  (pl :beg (min x y) :end (max x y)))
(defun sp-sel->nv-sel (s)
  (list :beg (plget s :beg) :end (plget s :end)))
(idefun nv-sel-at (&optional c)
        (if c
            (save-excursion (goto-char c) (sp-sel->nv-sel (sp-get-thing)))
          (sp-sel->nv-sel (sp-get-thing))))
(defun nv-point (p)
  (nv-sel p p))

(defun nv-beg (s)
  (plget s :beg))
(defun nv-end (s)
  (plget s :end))
(defun nv-bbeg (s)
  (1- (nv-beg s)))
(defun nv-aend (s)
  (1+ (nv-end s)))

(defun nv-str (s)
  (buffer-substring (nv-beg s) (nv-end s)))

(defun nv-beg! (s v)
  (nv-sel v (nv-end s)))
(defun nv-end! (s v)
  (nv-sel (nv-beg s) v))

(defun nv-beg!! (s)
  (nv-end! s (nv-beg s)))
(defun nv-end!! (s)
  (nv-beg! s (nv-end s)))

(defun nv-beg+ (s n)
  (nv-sel (v (+ n (nv-beg s))) (nv-end s)))
(defun nv-end+ (s n)
  (nv-sel (nv-beg s) (v (+ n (nv-end s)))))

(defun nv-size (s)
  (- (nv-end s) (nv-beg s)))
(defun nv-size= (s n)
  (equal n (nv-size s)))

(defun nv-empty? (s)
  (equal (nv-beg s) (nv-end s)))
(defun nv-unit? (s)
  (nv-size= s 1))

(defun nv-beg-char (s)
  (and (not (nv-empty? s)) (substring (nv-str s) 0 1)))
(defun nv-end-char (s)
  (and (not (nv-empty? s)) (substring (nv-str s) -1 0)))

(idefun nv-mark! (s)
        (goto-char (nv-beg s))
        (set-mark (nv-end s)))

;; native predicates
(defun nv-expr? (s)
  (and (member (nv-beg-char s) delimiters) s))
(defun nv-word? (s)
  (and (not (member (nv-beg-char s) delimiters))
       s))
(idefun nv-first? (s)
        (save-excursion
          (goto-char (nv-beg s))
          (when (sp-get-enclosing-sexp)
            (sp-backward-up-sexp)
            (sp-down-sexp)
            (and (equal s (nv-sel-at)) s))))
(idefun nv-last? (s)
  (save-excursion
    (goto-char (nv-beg s))
    (when (sp-get-enclosing-sexp)
      (sp-backward-up-sexp)
      (sp-forward-sexp)
      (sp-backward-down-sexp)
      (sp-backward-sexp)
      (and (equal s (nv-sel-at)) s))))
(defun nv-top? (s)
  (and (not (sp-get-enclosing-sexp)) s))
(defun nv-bottom? (s)
  (nv-word? s))

;; native moves
(idefun nv-prev (s)
        (and (not (nv-first? s))
             (save-excursion
               (goto-char (nv-beg s))
               (sp-backward-sexp)
               (nv-sel-at))))
(idefun nv-next (s)
        (and (not (nv-last? s))
             (save-excursion
               (goto-char (nv-beg s))
               (sp-forward-sexp 2)
               (sp-backward-sexp)
               (nv-sel-at))))
(idefun nv-out (s)
        (and (not (nv-top? s))
             (save-excursion
               (sp-backward-up-sexp)
               (nv-sel-at))))
(idefun nv-in (s)
        (and (not (nv-bottom? s))
             (save-excursion
               (goto-char (nv-beg s))
               (sp-down-sexp)
               (nv-sel-at))))

(comment (if-let ((nxt (funcall (nv-mv (cadr xs)) s)))
             (funcall (apply 'nv-mv (cdddr xs)) s)
           (funcall (apply 'nv-mv (cddr xs)) s)))

(defun nv-mv (&rest xs)

  (cond

   ((not xs) (fn (s) s))

   ((pb-kw? (car xs))
    (pcase (car xs)
      (:or
       (fn (s)
           (if-let ((nxt (call (cadr xs) s)))
               (call (apply 'nv-mv (cdddr xs)) nxt)
             (call (apply 'nv-mv (cddr xs)) s))))

      (:all
       (fn (s)
           (let* ((xs (cddr xs))
                  (s (seq-reduce
                      (fn (s m) (and s (call m s)))
                      (cadr xs)
                      s)))
             (call (apply 'nv-mv xs) s))))

      (:any
       (fn (s)
           (let* ((mvs (seq-into (cadr xs) 'list))
                  (xs (cddr xs))
                  (s (or (call (nv-mv (car mvs)) s)
                         (and (cdr mvs) (call (nv-mv :any (cdr mvs)) s)))))
             (call (apply 'nv-mv xs) s))))
      (:deep
       (fn (s)
           (let* ((mv (cadr xs))
                  (xs (cdr xs))
                  (s (if-let ((nxt (call mv s)))
                         (call (nv-mv :rec mv) nxt)
                       s)))
             (call (apply 'nv-mv xs) s))))
      (_ (error "Unknown expression %S" (car xs)))))

    (:else
    (fn (s)
        (call (apply 'nv-mv (cdr xs)) (call (car xs) s)) ))))

(comment "tests"
 (nv-mark-current! (call (nv-mv :or 'nv-next 'nv-prev) (nv-sel-at)))
 (nv-mark-current! (call (nv-mv :all [nv-next nv-prev]) (nv-sel-at)))
 (nv-mark-current! (call (nv-mv :any [nv-next nv-prev]) (nv-sel-at)))
 (nv-mark-current! (call (nv-mv :deep 'nv-prev) (nv-sel-at))))


;; deep moves
(idefun nv-deep-next (s)
        (let ((nxt (nv-next s)))
          (if (equal s nxt)
              nxt
            (nv-deep-next nxt))))
(idefun nv-deep-prev (s)
        (let ((nxt (nv-prev s)))
          (if (equal s nxt)
              nxt
            (nv-deep-next nxt))))
(idefun nv-deep-in ()
        (if (nv-expr? s) (nv-deep-in (nv-in s)) s))
(idefun nv-deep-out (s)
        (if (nv-top? s) s (nv-deep-out (nv-out s))))

;; nav moves
(idefun nv-fw (s)
        ())
(idefun nv-bw (s)
        (if (nv-first? s)
            (let ((pp (-> s nv-out nv-bw)))
              (if (nv-expr? pp)
                  (-> pp nv-in nv-last)
                pp))
          (nv-prev s)))
(idefun nv-ffw (s)
        (if (nv-last? s)
            (nv-fw s)
          (nv-last s)))
(idefun nv-fbw (s)
        (if (nv-first? s)
            (nv-bw s)
          (nv-first s)))
(idefun nv-fwin (s)
        (if (nv-expr? s)
            (nv-in s)
          (nv-fw s)))
(idefun nv-fwout (s)
        (-> s nv-out nv-fw))
(idefun nv-bwin (s)
        (-> s nv-bw nv-rlast))
(comment (a (b c d) (e f (g b)) e (((e) e) e) e))

(defvar nv-state
      (pl :current (pl :beg 0 :end 0)
          :reverse nil))

(idefun nv-set! (v)
  (setq nv-state v))
(idefun nv-current ()
  (plget nv-state :current))
(idefun nv-current! (&optional s)
        (let ((s (or s (nv-sel-at))))
          (nv-set! (plput nv-state :current s))
          s))
(idefun nv-upd-current! (f)
        (nv-current! (funcall f (nv-current))))
(idefun nv-reverse! ()
        (nv-set! (plupd nv-state :reverse 'not)))
(idefun nv-out! ()
        (nv-mark-current! (nv-out (nv-current))))
(idefun nv-in! ()
        (nv-mark-current! (nv-in (nv-current))))
(idefun nv-mark-current! (&optional s)
        (print "mark-current!") 
        (nv-mark! (if s (nv-current! s) (nv-current)))
        (when (plget nv-state :reverse)
          (exchange-point-and-mark)))
(idefun nv-splice-current! ()
        (let ((c (nv-current)))
          (nv-in c)
          (save-excursion
            (goto-char (nv-beg (nv-current)))
            (paredit-splice-sexp))
          (nv-mark-current! (nv-sel-at))))
(idefun nv-wrap-current! ()
        (goto-char (nv-beg (nv-current)))
        (paredit-wrap-round)
        (nv-current! (nv-sel-at))
        (nv-mark-current!))
(idefun nv-copy-current! ()
        (let ((c (nv-current)))
          (kill-ring-save (nv-beg c) (nv-end c))))
(idefun nv-kill-current! ()
        (let ((c (nv-current)))
          (cond
           ((nv-only? c) (sp-kill-sexp))
           ((nv-last? c)
            (sp-kill-sexp)
            (save-excursion
              (let ((p (point)))
                (re-search-backward "[^ \n]")
                (forward-char)
                (delete-region p (point))))
            (nv-mark-current! (nv-sel-at)))
           ('else
            (sp-kill-sexp)
            (save-excursion
              (let ((p (point)))
                (re-search-forward "[^ \n]")
                (backward-char)
                (delete-region p (point))))
            (nv-mark-current! (nv-sel-at))))))
(idefun nv-indent-current! ()
        (let* ((c (nv-current!))
               (c (if (nv-word? c) (nv-out c) c)))
          (indent-region (nv-beg c) (nv-end c))))
(idefun nv-paste! ()
        (nv-mark-current!)
        (call-interactively 'delete-region)
        (yank)
        (nv-current! (nv-sel-at)))
(defun nv-fold-cycle! (arg)
  (interactive "p")
  (let* ((c (nv-current))
         (fold-state? (plget c :fold-state))
         (fold-state (or fold-state? (if (hs-already-hidden-p) :folded :unfolded))))
    (cond
     ((equal fold-state :unfolded)
      (hs-hide-block)
      (nv-mark-current! (plput (nv-sel-at) :fold-state :folded)))
     ((equal fold-state :folded)
      (hs-hide-level arg)
      (nv-mark-current! (plput (nv-sel-at) :fold-state :unfolding)))
     ((equal fold-state :unfolding)
      (hs-show-block)
      (nv-mark-current! (plput (nv-sel-at) :fold-state :unfolded))))))

(progn "nav-mode"

  (defvar nav-mode-map (make-sparse-keymap))

  (defun init-escape-keys (x &rest xs)
    (defmks nav-mode-map x (ifn ()(nav-mode -1) (deactivate-mark)))
    (when xs (apply 'init-escape-keys xs)))
  (init-escape-keys
   "<space>" 
   "<escape>" 
   "<return>"
   "<backspace>" 
   "<left>" 
   "<right>" 
   "<down>" 
   "<up>"
   )

  (defmks nav-mode-map
    "i" (ifn () (nv-upd-current! 'nv-bw) (nv-mark-current!))
    "o" (ifn () (nv-upd-current! 'nv-fw) (nv-mark-current!))
    "u" (ifn () (nv-upd-current! 'nv-fbw) (nv-mark-current!)) 
    "p" (ifn () (nv-upd-current! 'nv-ffw) (nv-mark-current!))
    "j" (ifn () (nv-upd-current! 'nv-bwout) (nv-mark-current!))
    "m" (ifn () (nv-upd-current! 'nv-fwout) (nv-mark-current!)) 
    "k" (ifn () (nv-upd-current! 'nv-bwin) (nv-mark-current!))
    "l" (ifn () (nv-upd-current! 'nv-fwin) (nv-mark-current!))
    "h" (ifn () (nv-reverse!) (nv-mark-current!))

    "c" 'nv-copy-current!
    "w" 'nv-kill-current!
    "x" 'nv-kill-current!
    "v" 'nv-paste!
    "s" 'nv-splice-current!
    "q" 'nv-wrap-current!
    "<tab>" 'nv-fold-cycle!

    )

  (comment
   (+ 1 2 (aze baz [1 2] )
      (aze baz [1 2])
      (aze baz [1 2] )))

  (define-minor-mode nav-mode
    :init-value nil
    :lighter " Nav"
    :keymap nav-mode-map
    :group 'nav)
  (defks "s-j" (ifn () (nav-mode 1) (nv-current! (nv-sel-at)))))

(comment
 (abc 1
      3
      (+ 12
         ert
         (iop df 3)
         (z df 42)
         o)
      (p oi l)))


