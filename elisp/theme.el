;;; misterioso-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Sebastian Hermida

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme perso
  "addition to gotham theme, made with scala in mind")


(let ((class '((class color) (min-colors 89))))

  (custom-theme-set-faces
   'perso

   `(ensime-implicit-highlight ((,class (:background "#0c1014"))))

   ; `(font-lock-custom-face               ((,class (:foreground "#ff0000"))))
   `(font-lock-type-face                 ((,class (:foreground "#6a3e6f"))))
   `(font-lock-string-face               ((,class (:foreground "#DF8F4E"))))
   `(font-lock-constant-face             ((,class (:foreground "#387485"))))
   `(font-lock-keyword-face              ((,class (:foreground "#31515a"))))
   `(font-lock-comment-face              ((,class (:foreground "#4c424d" :background "#292b2e"))))
   `(font-lock-function-name-face        ((,class (:foreground "#756C77"))))
   `(font-lock-variable-name-face        ((,class (:foreground "#756C77" :bold t))))
   `(highlight-numbers-number            ((,class (:foreground "#c06534"))))
   `(font-lock-preprocessor-face         ((,class (:foreground "#ff8b4c"))))
   `(default                             ((,class (:foreground "#989a9a"))))
   `(link                                ((,class (:foreground "#8E66BA"))))
   `(trailing-whitespace                 ((,class (:background "#091f2e"))))
   `(font-lock-builtin-face              ((,class (:foreground "#1d78a3"))))

   `(scala-font-lock:abstract-face       ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:final-face          ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:implicit-face       ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:lazy-face           ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:override-face       ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:private-face        ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:protected-face      ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:sealed-face         ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:var-face            ((,class (:foreground "#df6f5b"))))
   `(scala-font-lock:var-keyword-face    ((,class (:foreground "#df6f5b"))))

   `(rainbow-delimiters-depth-1-face     ((,class (:foreground "#736d75"))))
   `(rainbow-delimiters-depth-2-face     ((,class (:foreground "#969197"))))
   `(rainbow-delimiters-depth-3-face     ((,class (:foreground "#aba7ac"))))
   `(rainbow-delimiters-depth-4-face     ((,class (:foreground "#736d75"))))
   `(rainbow-delimiters-depth-5-face     ((,class (:foreground "#969197"))))
   `(rainbow-delimiters-depth-6-face     ((,class (:foreground "#aba7ac"))))
   `(rainbow-delimiters-depth-7-face     ((,class (:foreground "#736d75"))))
   `(rainbow-delimiters-depth-8-face     ((,class (:foreground "#969197"))))
   `(rainbow-delimiters-depth-9-face     ((,class (:foreground "#aba7ac"))))
   `(rainbow-delimiters-depth-10-face     ((,class (:foreground "#736d75"))))
   `(rainbow-delimiters-depth-11-face     ((,class (:foreground "#969197"))))
   `(rainbow-delimiters-depth-12-face     ((,class (:foreground "#aba7ac"))))
   `(rainbow-delimiters-depth-13-face     ((,class (:foreground "#736d75"))))
   `(rainbow-delimiters-depth-14-face     ((,class (:foreground "#969197"))))
   `(rainbow-delimiters-depth-15-face     ((,class (:foreground "#aba7ac"))))
   `(rainbow-delimiters-unmatched-face   ((,class (:background "#000"))))

   ;; org

   `(org-level-1 ((,class (:foreground "#FFBC67"))))
   `(org-level-2 ((,class (:foreground "#EF9C72"))))
   `(org-level-3 ((,class (:foreground "#DA727E"))))
   `(org-level-4 ((,class (:foreground "#cf4a85"))))
   `(org-level-5 ((,class (:foreground "#DA727E"))))
   `(org-level-6 ((,class (:foreground "#EF9C72"))))
   `(org-level-7 ((,class (:foreground "#FFBC67"))))

   `(org-todo ((,class (:foreground "#FF564D"))))
   `(org-meta-line ((,class (:foreground "#5C4379"))))
   `(org-link ((,class (:foreground "#8E66BA"))))
   `(org-special-keyword ((,class (:foreground "#6F5079"))))
   `(org-tag ((,class (:foreground "#05635E"))))

   ;; dired
   `(dired-directory ((,class (:foreground "#DA727E"))))
   `(dired-header ((,class (:foreground "#6a3e6f"))))

   `(treemacs-directory-face ((,class (:inherit default))))
   `(treemacs-directory-collapsed-face ((,class (:inherit default))))
   `(treemacs-root-face ((,class (:inherit default :height 160 :bold t))))

   ;; clojure

   ;`(clojure-keyword-face ((,class (:foreground "#1d78a3"))))
   `(clojure-nsprefix-face ((,class (:bold t))))
   `(cider-repl-stderr-face ((,class (:inherit default))))
   `(cider-repl-stdout-face ((,class (:inherit default))))
   ;`(clojure-boolean-face ((,class (:foreground "#D446C3" :bold t))))
   ;`(clojure-character-face ((,class (:foreground "#BE1017"))))
   ;`(clojure-quotedsym-face ((,class (:foreground "#D45D64"))))

   ;; should be replaced
   `(font-lock-function-name-face ((,class (:foreground "#EB6635"))))
   `(font-lock-variable-name-face ((,class (:foreground "#EB6635" :bold t))))
   `(font-lock-doc-face ((,class (:foreground "#656666"))))

   ;`(clojure-coremacro-face ((,class (:foreground "yellow"))))
   ;`(clojure-fname-face ((,class (:foreground "pink" :bold t))))
   ;`(clojure-varname-face ((,class (:foreground "pink" :bold t))))
   ;`(clojure-type-face ((,class (:foreground "red"))))
   ;`(clojure-lambdargs-face ((,class (:foreground "purple"))))

   ;`(highlight-numbers-number ((,class (:foreground "#DEAEC3"))))
   ;`(link ((,class (:foreground "#8E66BA"))))

   ))

(custom-theme-set-variables
 'perso
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95"
			    "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"]))

(provide-theme 'perso)




;; Local Variables:
;; no-byte-compile: t
;; End:

;;; misterioso-theme.el  ends here

;(add-hook 'clojure-mode-hook
;          '(lambda ()
;             (font-lock-add-keywords
;              nil
;              '(("(\\(defcustom\\)\\s-+\\(\\w+\\)"
;                 (1 font-lock-custom-face)
;                 (2 font-lock-function-name-face)
;                 )))))
