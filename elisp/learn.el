;; maps ------------------------------------------------

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

;; exemples ------------------------

(progn
  (hput (hnew) :a 1 :b 2)
  (hget (hm :a 1 :b (hm :c 1)) :a)
  (hget (hm :a 1 :b (hm :c 1)) '(:b :c))
  (hrem (hm :a 1 :b 2 :c 3) :a :c))

;; Association lists ---------------------------------------

;; set x to a alist
(setq x
      '(("mary" . 23)
        ("john" . 24)
        ("smith" . 33)))

(assoc "john" x)
(cdr (assoc "john" x))
(rassoc 24 x)

(plist-get '(:a 1 :b 2) :a)
(plist-put '(:a 1 :b 2) :b 4)

;; basics -----------------------------------------------------------------

(intern "aze") ;-> 'aze
(symbol-name 'aze) ;-> "aze"

;; utils ------------------------------------------------------------------

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
  (macroexpand '(pb-cond a b c d (e f g) (h i j) k (l m o) (p q r) s)))

;; plists ----------------------------------------------------------------

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
              (pl :p 33)))

;; nav ------------------------------------------------------------------------------

;; vars
(defvar opening-delimiters '("(" "[" "{"))
(defvar closing-delimiters '(")" "]" "}"))
(defvar delimiters (apply 'append opening-delimiters closing-delimiters))

;; state
(defvar nv-sel '(:beg 0 :end 0))

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
            (dbg 'freezed nv-freeze-saved)
            (setq nv-sel nv-freeze-saved)
            (dbg 'freeze-ret nv-freeze-ret)
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

(defun nv-sel! (x y)
  (setq nv-sel `(:beg ,(min x y) :end ,(max x y))))

(defun nv-beg ()(plget nv-sel :beg))
(defun nv-end ()(plget nv-sel :end))
(defun nv-bbeg () (1- (nv-beg)))
(defun nv-aend () (1+ (nv-end)))

(defun nv-str ()(buffer-substring (nv-beg) (nv-end)))

(defun nv-beg! (v)(nv-sel! v (nv-end)))
(defun nv-end! (v)(nv-sel! (nv-beg) v))

(defun nv-beg!! ()(nv-end! (nv-beg)))
(defun nv-end!! ()(nv-beg! (nv-end)))

(defun nv-beg+ (n)(nv-beg! (+ n (nv-beg))))
(defun nv-end+ (n)(nv-end! (+ n (nv-end))))

(defun nv-size ()(- (nv-end) (nv-beg)))
(defun nv-size= (n)(equal n (nv-size)))

(defun nv-empty? ()(equal (nv-beg) (nv-end)))
(defun nv-unit? ()(nv-size= 1))

(defun nv-beg-char ()(and (not (nv-empty?)) (substring (nv-str) 0 1)))
(defun nv-end-char ()(and (not (nv-empty?)) (substring (nv-str) -1 0)))

(defun nv-expr? ()(member (nv-beg-char) delimiters))
(defun nv-word? ()(not (member (nv-beg-char) delimiters)))

(defun nv-first? ()(looking-back "[([{] *\n*"))
(defun nv-last? ()
  (save-excursion
    (sp-forward-sexp)
    (or (looking-at " *\n*[)}]")(looking-at " *\n*\]"))))

(defun nv-point! (&optional p) (nv-sel! (or p (point)) (or p (point))))
(defun nv-mark () (nv-point!) (save-excursion (sp-forward-sexp) (nv-end! (point))))

;; mvs

(comment "mvs overkill impl"

 (defvar nv-mv0 (pl :body 'id :before 'id :after 'id))

 (defun nv-mv (&rest spec)(plmrg nv-mv0 spec))
 (defun nv-dmv (mv &rest updspec)(apply 'plupd mv updspec))

 (defun nv-do-mv (mv)
   (funcall (plget mv :before))
   (funcall (plget mv :body))
   (funcall (plget mv :after)))

 (defmacro nv-defmv (name &rest spec)
   (let ((spec-sym (intern (concat (symbol-name name) "-spec"))))
     `(progn
        (defvar ,spec-sym (nv-mv ,@spec))
        (defun ,name () (nv-do-mv ,spec-sym)))))

 (defmacro nv-def-simple-mv (name &rest body)
   `(nv-defmv ,name :body (ifn () ,@body)))

 (defmacro nv-def-simple-mvs (&rest xs)
   `(progn
      ,@(mapcar (ifn (x) `(nv-def-simple-mv ,(car x) ,(cadr x)))
                (-partition 2 xs))))

 (progn "tests" 
        (macroexpand '(nv-defmv bob :body (ifn () (print "hi"))))
        (macroexpand '(nv-def-simple-mv ert (print "hi") (print "world")))
        (macroexpand '(nv-def-simple-mvs aze (print "a") baz (print "b"))))

 (nv-def-simple-mvs
  nv-prev (when (not (nv-first?))(sp-backward-sexp))
  nv-next (when (not (nv-last?))(sp-forward-sexp 2)(sp-backward-sexp))
  nv-first (when (not (nv-first?))(sp-backward-sexp)(nv-first))
  nv-last (nv-?> (not (nv-last?)) nv-next nv-last)
  nv-out (sp-backward-up-sexp)
  nv-in (nv-?> nv-expr? sp-down-sexp))
 
 (defvar nv-bw-mvs (list 'nv-prev 'nv-out 'nv-first))
 (defvar nv-fw-mvs (list 'nv-next 'nv-in 'nv-last))

 (idefun nv-mark-mv (mv)
         (nv-<
          (nv-bw-mv? mv)
          (nv-if nv-empty?
                 (nv-> nv-prev nv-mark)
                 (nv-beg-mv mv))
          (nv-fw-mv? mv)
          (nv-if nv-empty?
                 nv-mark
                 (nv-end-mv mv))))
)

(idefun nv-prev () (when (not (nv-first?))(sp-backward-sexp)))
(idefun nv-next () (when (not (nv-last?))(sp-forward-sexp 2)(sp-backward-sexp)))
(idefun nv-first () (when (not (nv-first?))(sp-backward-sexp)(nv-first)))
(idefun nv-last () (nv-?> (not (nv-last?)) nv-next nv-last))
(idefun nv-out () (sp-backward-up-sexp))
(idefun nv-in () (nv-?> nv-expr? sp-down-sexp))

(defun nv-bw-mv? (mv) (member mv nv-bw-mvs))
(defun nv-fw-mv? (mv) (member mv nv-fw-mvs))

(idefun nv-fresh-mv (mv) (nv-> nv-point! (funcall mv) nv-point!))
(idefun nv-marked-mv (mv) (nv-fresh-mv mv) (nv-mark))

(idefun nv-beg-mv (mv) (nv-beg! (nv-freeze (nv-fresh-mv mv) (nv-beg))))
(idefun nv-end-mv (mv)
        (dbg 'before nv-sel)
        (dbg 'moved (nv-freeze nv-end!! (nv-fresh-mv mv)))
        (nv-end! (plget (nv-freeze nv-end!! (nv-fresh-mv mv)) :end))
        (goto-char (nv-beg))
        (set-mark (nv-end))
        )

(defks
  "M-<left>" (ifn () (nv-fresh-mv 'nv-prev))
  "M-<right>" (ifn () (nv-fresh-mv 'nv-next))
  "C-M-<left>" (ifn () (nv-fresh-mv 'nv-first))
  "C-M-<right>" (ifn () (nv-fresh-mv 'nv-last))

  "M-S-<left>" (ifn () (nv-end-mv 'nv-prev))
  "M-S-<right>" (ifn () (nv-end-mv 'nv-next))
  "C-M-S-<left>" (ifn () (nv-beg-mv 'nv-prev))
  "C-M-S-<right>" (ifn () (nv-beg-mv 'nv-next)))

(comment
 (abc 1
      3
      (+ 12
         ert
         (iop df 3)
         (z df 42)
         o)
      (p oi l)))


