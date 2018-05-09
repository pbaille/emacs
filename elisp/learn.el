;;; learn.el --- lerning emacs etc...  -*- lexical-binding: t -*-

;; learning, exemples...  --------------------------------------------------

(comment "misc"

         ;;equivalent
         (defalias 'xp-id (lambda (a) a))
         (defun xp-id (a) a)

         (let ((a 1) (b 2)) (+ a b))

         (let* ((a 1) (b a)) (+ a b))

         (flet ((yop (a) "yop"))
           (yop 42))

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

;; vars
(defvar opening-delimiters
  '("(" "[" "{"))
(defvar closing-delimiters
  '(")" "]" "}"))
(defvar delimiters
  (apply 'append opening-delimiters closing-delimiters))

;; impl
(defun sp-sel->nv-sel (s)
  (list :beg (plget s :beg) :end (plget s :end)))

;; constructors
(defun nv-sel (x y)
  (pl :beg (min x y) :end (max x y)))
(idefun nv-sel-at (&optional c)
        (if c
            (save-excursion (goto-char c) (sp-sel->nv-sel (sp-get-thing)))
          (sp-sel->nv-sel (sp-get-thing))))
(defun nv-point (p)
  (nv-sel p p))

;; inspection
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

;; transformations
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

;; predicates
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

(defmacro pb-flet (bindings &rest body)
  (if-let ((b1 (car bindings)))
      `(cl-flet (,b1)
         (pb-flet ,(cdr bindings) ,@body))
    `(progn ,@body)))

(pb-flet ((inc (x) (1+ x))
          (plus2 (x) (-> x inc inc)))
         (plus2 1))
;; moves compositor

(defun list! (x) (seq-into x 'list))
(defun seq-apply (f xs)(apply f (list! xs)))

(defun nv-mv (&rest xs)

  (cl-flet

      ((cont (f xs)
             (fn (s)
                 (call (seq-apply 'nv-mv xs)
                       (call f s)))))

    (cond

     ((not xs) (fn (s) s))

     ((vectorp (car xs))
      (cont (cont 'pb-id (car xs))
            (cdr xs)))

     ((listp (car xs))
      (cont (cont 'pb-id (car xs))
            (cdr xs)))

     ((pb-kw? (car xs))
      (pcase (car xs)

        (:opt
         (cont
          (fn (s)(or (call (nv-mv (cadr xs)) s) s))
          (cddr xs)))
        (:or
         (cont
          (fn (s)(or (call (nv-mv (cadr xs)) s)
                     (call (nv-mv (caddr xs)) s)))
          (cdddr xs)))

        (:all
         (cont
          (fn (s) (call (seq-apply 'nv-mv (cadr xs)) s))
          (cddr xs)))

        (:any
         (cont
          (fn (s)
              (let ((mvs (seq-into (cadr xs) 'list)))
                (or (call (nv-mv (car mvs)) s)
                    (and (cdr mvs) (call (nv-mv :any (cdr mvs)) s)))))
          (cddr xs)))

        (:deep
         (cont
          (fn (s)
              (if-let ((mv (cadr xs))
                       (nxt (call (nv-mv mv) s)))
                  (call (nv-mv :deep mv) nxt)
                s))
          (cddr xs)))

        (_ (error "Unknown expression %S" (car xs)))))

     (:else
      (cont (car xs) (cdr xs))))))

(comment "tests"
         (call (nv-mv 'nv-next) (nv-sel-at))
         (call (nv-mv 'nv-next 'nv-next) (nv-sel-at))
         (call (nv-mv '(nv-next nv-prev)) (nv-sel-at))
         (call (nv-mv [nv-next nv-prev]) (nv-sel-at))
         (call (nv-mv :or 'nv-next 'nv-prev) (nv-sel-at))
         (call (nv-mv :all [nv-next nv-prev]) (nv-sel-at))
         (call (nv-mv :any [nv-next nv-prev]) (nv-sel-at))
         (call (nv-mv :deep 'nv-next) (nv-sel-at)))

;; conveniance macro 
(defmacro nv-defmv (name mvs)
  `(defalias ',name
     (nv-mv ,(seq-into mvs 'vector))))

;; deep moves
(nv-defmv nv-deep-next [:deep nv-next])
(nv-defmv nv-deep-prev [:deep nv-prev])
(nv-defmv nv-deep-out [:deep nv-out])
(nv-defmv nv-deep-in [:deep nv-in])

(defun nv-dbgmv (s) (dbg 'mv-dbg s) s)

;; nav moves
(nv-defmv nv-fw [:any [nv-next [nv-out nv-next :opt nv-in] (fn (s) (dbg 'dodu s) nil)]])
(nv-defmv nv-bw [:or nv-prev [nv-out nv-bw :opt [nv-in nv-deep-next]]])
(nv-defmv nv-ffw [:or nv-deep-next [nv-out nv-fw :opt nv-in]])
(nv-defmv nv-fbw [:or nv-deep-prev [nv-out nv-bw :opt [nv-in nv-deep-next]]])
(nv-defmv nv-fwin [:or nv-in nv-fw])
(nv-defmv nv-bwin [:any [[nv-bw nv-in nv-deep-next] [nv-bw nv-in] nv-bw]])
(nv-defmv nv-fwout [nv-dbgmv :or [nv-out nv-fw] [nv-out nv-fwout]])

(comment (a (b c d) (e f (g b)) e (((e) e) e) e))

;; state
(defvar nv-state
      (pl :current (pl :beg 0 :end 0)
          :reverse nil))

;; side effetcts
(idefun nv-mark! (s)
        (goto-char (nv-beg s))
        (set-mark (nv-end s)))
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
(idefun nv-mark-current! (&optional s) 
        (nv-mark! (if s (nv-current! s) (nv-current)))
        (when (plget nv-state :reverse)
          (exchange-point-and-mark)))
(idefun nv-splice-current! ()
        (let ((c (nv-current)))
          (nv-in c)
          (save-excursion
            (goto-char (nv-beg (nv-current)))
            (paredit-splice-sexp))
          (nv-mark-current!)))
(idefun nv-wrap-current! ()
        (goto-char (nv-beg (nv-current)))
        (paredit-wrap-round)
        (nv-current!)
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
        (nv-current!))
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
    "j" (ifn () (nv-upd-current! 'nv-out) (nv-mark-current!))
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


