
(setq clj-minor ())

(define-minor-mode clj-minor
  "personal addition to clojure-mode"
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " my-clj"
  ;; The minor mode bindings.
  :keymap
  '(([C-backspace] . clj-minor-action1)
    )
  :group 'clj-minor-group)

(idefun clj-minor-action1 ()
  (print "yopyop"))

(idefun cj-minor-action1 ()
  (print "hello minor clj"))

