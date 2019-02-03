(autothemer-deftheme darkbooth+ "Autothemer example..."

  ;; Specify the color classes used by the theme
  ((((class color) (min-colors #xFFFFFF))
    ((class color) (min-colors #xFF)))

    ;; Specify the color palette for each of the classes above.
   (cyan "#00ede1" nil)
   (yellow "#ffad29" nil)
   (green "#74af68" nil)
   (dark-green "#008b8b" nil)
   (orange "#e67128" nil)
   )

    ;; specifications for Emacs faces.
  ((font-lock-builtin-face (:foreground dark-green))
   (font-lock-comment-face (:foreground green))
   (font-lock-constant-face (:foreground dark-green))
   (font-lock-function-name-face (:foreground cyan))
   (font-lock-keyword-face (:foreground yellow :weight 'bold))
   (font-lock-string-face (:foreground orange))
   (font-lock-type-face (:foreground dark-green :weight 'bold))
   (font-lock-variable-name-face (:foreground cyan))
   (font-lock-doc-face (:foreground orange :bold t))

   (treemacs-directory-face (:inherit 'font-lock-constant-face))
   (treemacs-root-face (:foreground yellow :bold t :height 170))
   (treemacs-file-face (:inherit 'default))

   )

    ;; Forms after the face specifications are evaluated.
    ;; (palette vars can be used, read below for details.)
  (custom-theme-set-variables 'darkbooth+
        `(ansi-color-names-vector [])))

