
;; learning, exemples...  --------------------------------------------------

(comment "ultra basics"
 (intern "aze")
 (symbol-name 'aze))

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
(idefun nv-only? (s)
        (and (nv-first? s) (nv-last? s) s))
(defun nv-top? (s)
  (and (not (sp-get-enclosing-sexp)) s))
(defun nv-bottom? (s)
  (nv-word? s))

(idefun nv-mark! (s)
        ;(deactivate-mark)
        ;(dbg 'nvmark s)
        (goto-char (nv-beg s))
        (set-mark (nv-end s))
        ;(pouet) nescessary to maintain region
        )

(idefun nv-prev (s)
        (or (nv-first? s)
            (save-excursion
              (goto-char (nv-beg s))
              (sp-backward-sexp)
              (nv-sel-at))))
(idefun nv-next (s)
        (or (nv-last? s)
            (save-excursion
              (goto-char (nv-beg s))
              (sp-forward-sexp 2)
              (sp-backward-sexp)
              (nv-sel-at))))
(idefun nv-first (s)
        (or (nv-first? s)
            (let ((nxt (nv-prev s)))
              (and (not (equal s nxt)) (nv-first nxt)))
            s))
(idefun nv-last (s)
        (or (nv-last? s)
            (let ((nxt (nv-next s)))
              (and (not (equal s nxt)) (nv-last nxt)))
            s))
(idefun nv-out (s)
        (or (nv-top? s)
            (progn (sp-backward-up-sexp) (nv-sel-at))))
(idefun nv-in (s)
        (or (nv-bottom? s)
            (save-excursion
              (goto-char (nv-beg s))
              (sp-down-sexp)
              (nv-sel-at))))
(idefun nv-fw (s)
        (if (nv-last? s)
            (-> s nv-out nv-fw nv-in)
          (nv-next s)))
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
        (dbg 'nv-first? (nv-first? s))
        (if (nv-first? s)
            (nv-bw s)
          (nv-first s)))
(idefun nv-fwin (s)
        (if (nv-expr? s)
            (nv-in s)
          (nv-fw s)))
(idefun nv-bwout (s)
        (if (nv-top? s)
            (nv-bw s)
          (nv-out s)))

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
    "i" (ifn () (nv-upd-current! 'nv-bw) (nv-mark-current!)) ;(nv-marked-mv 'nv-prev)
    "o" (ifn () (nv-upd-current! 'nv-fw) (nv-mark-current!))
    "u" (ifn () (nv-upd-current! 'nv-fbw) (nv-mark-current!)) 
    "p" (ifn () (nv-upd-current! 'nv-ffw) (nv-mark-current!))
    "j" (ifn () (nv-upd-current! 'nv-bwout) (nv-mark-current!))
    "m" (ifn () (nv-upd-current! 'nv-fwin) (nv-mark-current!)) 
    "k" (ifn () (nv-current! (-> (nv-current) nv-out nv-next)) (nv-mark-current!))
    "l" (ifn () (nv-current! (-> (nv-current) nv-in nv-last)) (nv-mark-current!))
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


