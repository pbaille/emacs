(autothemer-deftheme clojure+ "Autothemer example..."

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
  ((clojure-keyword-face (:foreground dark-green))
   (clojure-boolean-face (:foreground "red"))
   (clojure-quotedsym-face (:foreground green))
   (clojure-coremacro-face (:foreground yellow))
   (clojure-varname-face (:foreground cyan))
   (clojure-fname-face (:foreground cyan))
   (clojure-nsprefix-face (:bold t))
   (clojure-character-face (:foreground orange))
   (clojure-type-face (:foreground "blue"))
   (clojure-lambdargs-face (:foreground "gold"))
   (clojure-docstring-face (:foreground "white"))
   (clojure-regex-face (:foreground "violet"))))

