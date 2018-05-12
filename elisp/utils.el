(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defmacro defk (k f)
  (declare (indent defun))
  (list 'global-set-key (list 'kbd k) f))

(defk "s-K" 'erase-messages)

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

(defun call (f &rest args)
  (declare (indent defun))
  (apply 'funcall f args))

(defmacro mexp (f) `(macroexpand ,f))

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

(comment 
 (mexp '(ifn (a b) (+ a b)))
 (fn (a b) (+ a b))
 (ifn (a b) (+ a b)))

