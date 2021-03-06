(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe           ""
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries            5000
          treemacs-no-png-images              nil
          treemacs-no-delete-other-windows    t
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor                nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      25)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 44)

    (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-="       . treemacs-select-window)
        ("M-ù" . 'previous-buffer)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("s-:"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(comment
 (with-eval-after-load "treemacs"
   (setq treemacs-icon-open-png   (propertize "⊖ " 'face 'treemacs-directory-face)
         treemacs-icon-closed-png (propertize "⊕ " 'face 'treemacs-directory-face)
         ;; treemacs-icon-clojure (propertize " " 'face 'treemacs-file-face)

         )))

(comment

 (defmacro user-faces (&rest xs)
   `(let ((class '((class color) (min-colors 89))))
      (custom-set-faces
       ,@(seq-map
          (fn (x)
              ``(,,(car x) ((,class ,,(cdr x)))))
          xs))))

 (macroexpand-1
  '(user-faces
    (iop :a 1 :b 2)))

 (user-faces
  (treemacs-directory-face :foreground "green")))

(comment
 (let ((class '((class color) (min-colors 89))))
   (custom-set-faces
    `(treemacs-directory-face ((,class (:foreground "#aeafaf"))))
    `(treemacs-directory-collapsed-face ((,class (:foreground "#aeafaf"))))
    `(treemacs-file-face ((,class (:foreground "#989a9a" :height 120))))
    `(treemacs-root-face ((,class (:foreground "#aeafaf" :bold t :height 170)))))))

(comment
 (all-the-icons-icon-for-file "foo.clj"))
