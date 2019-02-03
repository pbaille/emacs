(progn "macros"

       (defmacro comment (&rest body)
         "Comment out one or more s-expressions."
         nil)

       (defmacro fn (&rest xs)
         `(lambda ,@xs))

       (defmacro ifn (args &rest xs)
         `(fn ,args (interactive) ,@xs))

       (defmacro idefun (name args &rest xs)
         (declare (indent defun))
         `(defun ,name ,args (interactive) ,@xs))

       (font-lock-add-keywords
        'emacs-lisp-mode
        '(("(\\(idefun\\)\\_>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
           (1 font-lock-keyword-face nil t)
           (2 font-lock-function-name-face nil t))))

       ;; (cdr font-lock-keywords-alist)

       ;; (pp-eval-expression 'font-lock-keywords-alist)

       ;; (pp-eval-expression)

       (defmacro mexp (f) `(macroexpand ,f))

       (comment
        (mexp '(ifn (a b) (+ a b)))
        (fn (a b) (+ a b))
        (ifn (a b) (+ a b)))

       ;; key bindings 
       (defmacro defk (k f)
         (declare (indent defun))
         (list 'global-set-key (list 'kbd k) f))

       (defmacro defkm (m k f)
         (declare (indent defun))
         (list 'define-key ,m (list 'kbd k) f))

       (defmacro defks (k f &rest kfs)
         (declare (indent defun))
         (if kfs
             `(progn (defk ,k ,f) (defks ,@kfs))
           `(defk ,k ,f)))

       (defmacro defkms (m k f &rest kfs)
         (declare (indent defun))
         (if kfs
             `(progn (defkm ,m ,k ,f) (defkms ,m ,@kfs))
           `(defkm ,m ,k ,f)))

       )

(progn "misc"

       (defun call (f &rest args)
         (declare (indent defun))
         (apply 'funcall f args))

       (defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
         "cleanup whitespace on kill-line"
         (if (not (bolp))
             (delete-region (point) (progn (skip-chars-forward " \t") (point))))))

(progn "paths"

       (defun path/str->segments (p)
         (split-string p "/"))

       (defun path/file? (p)
         (let* ((ss (path/str->segments p))
                (last-seg (car (last ss))))
           (and (cdr (split-string last-seg "\\.")) t)
           ))

       (defun path/split-filename (p)
         (let ((ss (path/str->segments p)))
           (if (path/file? p)
               (cons (string-join (butlast ss) "/") (car (last ss)))
             (list p))))

       (defun path/segments->str (ss)
         (string-join ss "/"))

       (defun path/parent (p)
         (path/segments->str
          (butlast (path/str->segments p))))

       (comment
        (path/parent (buffer-file-name (current-buffer)))
        (path/file? "aze/aze/aze")
        (path/split-filename "aze/aze/aze")
        (path/split-filename "aze/aze/aze.aze")))

(progn "seqs"

       (defun seq-starts (l)
         (if (eq 1 (length l))
             (list l)
           (cons l (seq-starts (butlast l)))))

       (defun seq-keep (f l)
         (seq-filter (fn (a) a) (seq-map f l))))


