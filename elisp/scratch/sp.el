;; provide sp depth first walking almost...
;; if interested see nav/nav.el

(defmacro sn-thing-op (name args &rest body)
  `(idefun ,name ,args
     (let ((,(car args) (or ,(car args) (sp-get-thing))))
       ,@body)))

(defun sn-type (x)
  (let ((op (sp-get x :op))
        (pr (sp-get x :prefix)))
    (cond
     ((equal "" op) :sym)
     ((or (equal op "[")
          (equal op "{")
          (equal op "(")) :sexp)
     ((equal "\"" op) :str))))

(defun sp-type= (thing type)
  (equal type (sp-type thing)))

(defun sp-beg (thing)
  (sp-get thing :beg))

(defun sp-end (thing)
  (- (sp-get thing :end) 1))

(defun sp-thing (&optional thing)
  (or thing (sp-get-thing)))

(sp-thing-op sp-print (thing)
  (print (buffer-substring (sp-get thing :beg) (sp-get thing :end))))

(sp-thing-op sp-beg-thing? (thing)
  (equal (point)
         (sp-get thing :beg)))

(idefun sp-end-thing? (thing)
  (equal (point)
         (- (sp-get thing :end) 1)))

(idefun sp-empty-sexp? (thing)
  (and
   (sp-type= thing :sexp)
   (equal  (+ 2 (sp-get thing :beg))
           (sp-get thing :end))))

(idefun sp-wrap-select (mv)
  (deactivate-mark)
  (funcall mv)
  (sp-mark-sexp))

(progn "fw"

       (sp-thing-op sp-force-fw (thing)
         (let ((next-thing (progn (sp-next-sexp) (sp-get-thing))))
           (when (> (sp-get thing :beg)
                    (sp-get next-thing :beg))
             (sp-force-fw next-thing))))

       (idefun sp-depth-first-fw ()
         (let* ((thing (sp-get-thing))
                (type (sp-type thing)))
           (cond
            ((not (sp-beg-thing? thing)) (goto-char (sp-get thing :beg)) (sp-force-fw))
            ((or (equal type :sym)
                 (equal type :str)) (sp-force-fw))
            ((sp-empty-sexp? thing) (sp-force-fw))
            ((equal type :sexp) (forward-char 1)))))

       (idefun sp-dffw ()
         (sp-wrap-select 'sp-depth-first-fw)))

(progn "bw"

       (idefun sp-force-bw (&optional thing)
         (let ((thing (sp-thing thing))
               (prev-thing (progn (sp-backward-sexp)
                                  (sp-previous-sexp)
                                  (backward-char 1)
                                  (sp-get-thing))))

           (print "-----------")
                                        ;(print (cons "thing: " thing))
           (sp-print thing)
                                        ;(print (cons "prev thing: " prev-thing))
           (sp-print prev-thing)

           (when (> (sp-get thing :beg)
                    (sp-get prev-thing :beg))
             (sp-force-bw prev-thing)
             )))

       (comment
        (aaa bbb ccc (ddd eee (fff ggg) (ooo ppp))))

       (idefun sp-depth-first-bw ()
         (let* ((thing (sp-get-thing))
                (type (sp-type thing)))
           (cond
            ((not (sp-end-thing? thing)) (goto-char (- (sp-get thing :end) 1)) (sp-force-bw))
            ((or (equal type :sym)
                 (equal type :str)) (sp-force-bw))
            ((sp-empty-sexp? thing) (sp-force-bw))
            ((equal type :sexp) (backward-char 1)))))

       (idefun sp-dfbw ()
         (print "dfbw:---------------------")
         (sp-wrap-select 'sp-depth-first-bw))
       )
(progn "mode"

  (defvar snav-mode-map (make-sparse-keymap))

  (defun init-escape-keys (x &rest xs)
    (defmks snav-mode-map x (ifn ()(snav-mode -1) (deactivate-mark)))
    (when xs (apply 'init-escape-keys xs)))
  (init-escape-keys
   "<escape>" 
   "<down>" 
   "<up>"
   )

  (defmks snav-mode-map
    "j" 'sp-dfbw
    ;; 'sp-previous-sexp ;;
    ;; 'sp-depth-first-bw
    ;; "l" 'sp-depth-first-fw
    "l" 'sp-dffw
    "i" 'sp-print)

  (define-minor-mode snav-mode
    :init-value nil
    :lighter " SNAV"
    :keymap snav-mode-map
    :group 'snav)

  (defks
    "s-j" (ifn ()
               (snav-mode 1)
               ;; TODO mark
               )))
