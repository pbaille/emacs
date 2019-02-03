;;; gotham-theme.el --- A very dark Emacs color theme.

;; Copyright (C) 2014-2017 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/gotham-theme
;; Package-Version: 20171013.1216
;; Version: 1.1.8

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A port of the Gotham theme for Vim to Emacs 24 that uses the new
;; theming support.

;;; Credits:

;; Andrea Leopardi created the original Vim theme on which this port
;; is based, it can be found under
;; <https://github.com/whatyouhide/vim-gotham>

;;; Code:

(defgroup tem nil
  "A very dark Emacs theme"
  :group 'themes
  :prefix "tem-")

(deftheme tem "personal theme")

(defvar tem-color-alist
  `((c-a1 . "#fd6ebd")
    (c-a2 . "#fd80c5")
    (c-a3 . "#fd92cd")
    (c-a4 . "#fea4d6")

    (c-b1 . "#4a8cfd")
    (c-b2 . "#427ee3")
    (c-b3 . "#3b70ca")
    (c-b4 . "#3362b1")

    (c-c1 . "#320e22")
    (c-c2 . "#4b1633")
    (c-c3 . "#651d45")
    (c-c4 . "#7e2556"))
  "List of colors the theme consists of.")



(defvar greens
  '((green-1  . "#ecf6f6")
    (green-2  . "#d9eeee")
    (green-3  . "#c6e5e6")
    (green-4  . "#b3ddde")
    (green-5  . "#a0d5d6")
    (green-6  . "#8dccce")
    (green-7  . "#7ac4c6")
    (green-8  . "#66bbbe")
    (green-9  . "#54b3b6")
    (green-10 . "#41abae")
    (green-11 . "#3a999c")
    (green-12 . "#34888b")
    (green-13 . "#2d7779")
    (green-14 . "#276668")
    (green-15 . "#205557")
    (green-16 . "#1a4445")
    (green-17 . "#133334")
    (green-18 . "#0d2222")
    (green-19 . "#061111")
    (green-20 . "#000000")))

(defun tem-set-faces (faces)
  "Helper function that sets the faces of the theme to a list of FACES.
See `gotham-transform-face' for the transformation, see
`gotham-transform-spec' for the rules."
  (apply #'custom-theme-set-faces 'tem
         (mapcar #'gotham-transform-face faces)))

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

       (defun gotham-transform-spec (spec display)
         "Helper function that transforms SPEC for DISPLAY.
DISPLAY is either 'graphic or 'tty, SPEC is a property list where
the values are substituted with colors from `gotham-color-alist'
depending on DISPLAY for keys which are either :foreground or
:background.  All other key-value combinations remain unchanged."
         (let (output)
           (while spec
             (let* ((key (car spec))
                    (value (cadr spec))
                    (color (cdr (assoc value tem-color-alist))))
               (cond
                ((and (memq key '(:box :underline)) (listp value))
                 (setq output (append output
                                      (list key (gotham-transform-spec value display)))))
                ((and (memq key '(:foreground :background :underline :overline :color))
                      color)
                 (setq output (append output (list key color))))
                (t (setq output (append output (list key value))))))
             (setq spec (cddr spec)))
           output)))

(tem-set-faces
 '((default :foreground c-a1 :background c-c1)
;;   (button :foreground base4 :box t)
   ;;   (shadow :foreground base4)
   ;;(highlight :background c-c1)
;;   (link :foreground orange :underline t)
;;   (link-visited :foreground yellow)
;;   (cursor :background base6)
;;   (region :foreground unspecified :background base3)
;;   (secondary-selection :foreground unspecified :background violet)
;;   (linum :foreground base4 :background base1)
;;   (line-number :foreground base4 :background base1)
;;   (line-number-current-line :inherit highlight)
;;   (fringe :foreground base1 :background base1)
;;   (vertical-border :foreground base4)
;;   (tooltip :foreground base6 :background base0)
    (trailing-whitespace :background c-c2)
;;
   ;;;; font-lock
   ;;(escape-glyph :foreground orange :weight bold)
   (font-lock-builtin-face :foreground c-b1)
   (font-lock-comment-face :foreground c-c4 :background c-c1)
   ;;(font-lock-comment-delimiter-face :foreground base4)
   ;;(font-lock-constant-face :foreground cyan :weight bold)
   ;;(font-lock-doc-face :foreground green)
   ;;(font-lock-function-name-face :foreground base5)
   ;;(font-lock-keyword-face :foreground blue :weight bold)
   ;;(font-lock-negation-char-face :foreground red)
   ;;(font-lock-preprocessor-face :foreground red)
   ;;(font-lock-regexp-grouping-construct :foreground yellow)
   ;;(font-lock-regexp-grouping-backslash :foreground yellow)
   (font-lock-string-face :foreground c-b4)
   ;;(font-lock-type-face :foreground orange)
   ;;(font-lock-variable-name-face :foreground base5)
   ;;(font-lock-warning-face :foreground red)
   ;;(error :foreground red)
   ;;(success :foreground green)
   ;;(warning :foreground orange)

   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'tem)

;;; tem-theme.el ends here
