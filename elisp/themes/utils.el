(defun set-faces (theme faces)
  "Helper function that sets the faces of the theme to a list of FACES.
See `gotham-transform-face' for the transformation, see
`gotham-transform-spec' for the rules."
  (apply #'custom-theme-set-faces theme
         (mapcar #'gotham-transform-face faces)))

(comment
 ;; (set-faces 'misterioso '((default :foreground "grey")))
 )

(progn "gotham utils"

       (defun gotham-transform-face (face)
         "Helper function that transforms a FACE to all variants.
FACE is a list where the first element is the name of the
affected face and the remaining elements specify the face
attributes which are transformed into face attributes for both
graphical and terminal displays.  See `gotham-transform-spec' for
the rules that are applied to the face attributes."
         (let* ((name (car face))
                (spec (cdr face))
                (graphic-spec (gotham-transform-spec spec 'graphic))
                (tty-spec (gotham-transform-spec spec 'tty)))
           `(,name ((((type graphic)) ,@graphic-spec)
                    (((type tty)) ,@tty-spec)))))

       (defun gotham-transform-spec (spec display &optional colors)
         "Helper function that transforms SPEC for DISPLAY.
DISPLAY is either 'graphic or 'tty, SPEC is a property list where
the values are substituted with colors from `gotham-color-alist'
depending on DISPLAY for keys which are either :foreground or
:background.  All other key-value combinations remain unchanged."
         (let (output)
           (while spec
             (let* ((key (car spec))
                    (value (cadr spec))
                    (color (cdr (assoc value (or colors nil)))))
               (cond
                ((and (memq key '(:box :underline)) (listp value))
                 (setq output (append output
                                      (list key (gotham-transform-spec value display colors)))))
                ((and (memq key '(:foreground :background :underline :overline :color))
                      color)
                 (setq output (append output (list key color))))
                (t (setq output (append output (list key value))))))
             (setq spec (cddr spec)))
           output)))


(idefun what-face ()
  (let* ((pos (point))
         (face (or (get-char-property pos 'read-face-name)
                   (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
