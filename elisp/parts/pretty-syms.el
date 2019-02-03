
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq prettify-symbols-unprettify-at-point t)
            (setq prettify-symbols-alist
                  '(("lambda" . 955)
                    ("progn" . ">-" )
                    ;; ("idefun" . "ƒ")
                    ;; ("defun" . "ƒ")
                    ))))

(global-prettify-symbols-mode 1)

