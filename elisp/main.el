;;; main.el --- main config  -*- lexical-binding: t -*-

;; terminal buffer send text ------------------------------------

(load "~/Code/Emacs/elisp/utils.el")

;; should kill this dep
(require 'paredit)

;; init --------------------------------------------------------

(progn

  "init"

  ;; completion
  (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)

  ;; smooth scroll
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 3) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)

  ;; mark relative
  (setq use-empty-active-region t)
  (setq mark-even-if-inactive t)
  (setq transient-mark-mode t)

  ;; bypass confirmation on kill buffer
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  ;; style
  (set-face-attribute 'default (selected-frame) :height 130)
  ;(set-fringe-style 'no-fringes)
  (setq neo-theme 'arrow)

  ;; for the minibuffer not to extend
  (setq resize-mini-windows nil)

  )

;; constants --------------------------------------------------

(defvar opening-delimiters '("(" "[" "{"))
(defvar closing-delimiters '(")" "]" "}"))
(defvar delimiters (apply 'append opening-delimiters closing-delimiters))

;; helpers ----------------------------------------------------

(progn "help"

  (defun dbg (&rest xs)
    (print (list* xs)))

  (idefun defmks (m k f &rest xs)
          (define-key m (kbd k) f)
          (when xs (apply 'defmks m xs)))

  (idefun empty-line (&optional n)
          (save-excursion
            (beginning-of-line (or n 1))
            (looking-at "[[:space:]]*$")))

  (idefun nxt-empty-line-dist (n)
          (if (not (empty-line n))
              (nxt-empty-line-dist (1+ n))
            n))

  (idefun prev-empty-line-dist (n)
          (if (not (empty-line n))
              (prev-empty-line-dist (1- n))
            n))

  (defun match-parenthesis (arg)
    (interactive "p")
    (let
        ((syntax (char-syntax (following-char))))
      (cond
       ((= syntax ?\()
        (forward-sexp 1) (backward-char))
       ((= syntax ?\))
        (forward-char) (backward-sexp 1))
       (t (message "No match")))))

  (defun compact-string (s)
    (replace-regexp-in-string
     "[ \n\t]+" " " s))

  (defun remove-lisp-comments (s)
    (replace-regexp-in-string
     ";[^\n]+\n*" "" s))

  (idefun substr+ret (start end)
          (-> (buffer-substring-no-properties start end)
              remove-lisp-comments
              compact-string
              (concat "\r"))))

;; selections -------------------------------------------------

(progn "selections"

  (idefun sels-get ()
          (when (and mark-active
                     (mark)
                     (point)
                     (not (equal (point) (mark))))
            (buffer-substring (mark) (point))))

  (idefun sels-empty? ()
          (not (sels-get)))

  (idefun sels-unmark ()
          (set-mark nil))

  (idefun sels-start ()
          (min (mark) (point)))

  (idefun sels-end ()
          (max (mark) (point)))

  (idefun sels-goto-beg ()
          (let ((p (sels-start))
                (m (sels-end)))
            (goto-char p)
            (set-mark m)))

  (idefun sels-goto-end ()
          (let ((p (sels-start))
                (m (sels-end)))
            (goto-char m)
            (set-mark p)))

  (idefun sp-mark (se)
          (set-mark (sp-get se :end))
          (goto-char (sp-get se :beg)))

  (idefun expand-region-to-enclosing-empty-lines ()
          (sels-goto-beg)
          (re-search-backward "\n *\n")
          (forward-char 2)
          (sels-goto-end)
          (re-search-forward "\n *\n")
          (backward-char 2))

  (idefun se-expand-mark ()
          (dbg "sels" (sels-get))
          (if (sels-get)
              (if-let ((se (sp-get-enclosing-sexp)))
                  (sp-mark se)
                (expand-region-to-enclosing-empty-lines))
            (sp-mark (sp-get-thing))))

  (idefun se-expand-max ()
          (print "expand max!")
          (se-expand-mark)
          (when-let ((se (sp-get-enclosing-sexp)))
            (sp-mark se)
            (se-expand-max)))
  )

;; point ------------------------------------------------------

(idefun current-char ()

  (buffer-substring
   (point)
   (min (point-max) (1+ (point)))))

(idefun get-prev-char ()
        (buffer-substring
         (max (point-min) (1- (point)))
         (point)))

(idefun get-next-char ()
        (buffer-substring
         (min (point-max) (1+ (point)))
         (min (point-max) (2 + (point)))))

(idefun current-char? (c)
  (equal (current-char) c))

(progn "point position" 

  (idefun point-on-opening-delimiter? ()
          (member (current-char) opening-delimiters))

  (idefun point-after-opening-delimiter?  ()
          (member (get-prev-char) opening-delimiters))

  (idefun point-before-opening-delimiter?  ()
          (member (get-next-char) opening-delimiters))

  (idefun point-on-closing-delimiter? ()
          (member (current-char) closing-delimiters))

  (idefun point-after-closing-delimiter?  ()
          (member (get-prev-char) closing-delimiters))

  (idefun point-before-closing-delimiter?  ()
          (member (get-next-char) closing-delimiters))

  (idefun point-on-delimiter? ()
          (or (point-on-opening-delimiter?)
              (point-on-closing-delimiter?)
              (equal "\"" (current-char))))

  (idefun point-on-space? ()
          (equal " " (current-char)))

  (idefun point-on-newline? ()
          (equal "\n" (current-char)))

  (idefun point-on-first-char-of-line? ()
          (save-excursion
            (backward-char)
            (cond
             ((point-on-newline?) t)
             ((point-on-space?) (point-on-first-char-of-line?)))))

  (idefun point-on-last-char-of-line? ()
          (save-excursion
            (forward-char)
            (point-on-newline?)))

  (idefun point-on-space-or-newline? ()
          (or (point-on-space?)
              (point-on-newline?)))

  (comment
   (idefun word-beginning? ()
           (when (and (not (delimiter?))
                      (not (space-or-newline?)))
             (let ((current (point))
                   (case-fold-search t))
               (save-excursion
                 (skip-chars-backward "\-a-zA-Z0-9")
                 (equal current (point)))))))

  (idefun point-on-symbol? ()
          (and (not (point-on-space-or-newline?))
               (not (point-on-delimiter?))))

  (idefun point-on-beginning-of-symbol? ()
          (and (point-on-symbol?)
               (save-excursion
                 (backward-char)
                 (or (point-on-space-or-newline?)
                     (point-on-delimiter?)))))

  )

(progn "scan" 

  (defun get-next-char (&optional ignore-blanks)
    (save-excursion
      (forward-char)
      (if ignore-blanks
          (or (and (point-on-space-or-newline?)
                   (get-next-char ignore-blanks))
              (current-char))
        (current-char))))

  (defun get-prev-char (&optional ignore-blanks)
    (save-excursion
      (backward-char)
      (if ignore-blanks
          (or (and (point-on-space-or-newline?)
                   (get-prev-char ignore-blanks))
              (current-char))
        (current-char))))

  (defun next-char-is (x &optional ignore-blanks)
    (equal x (get-next-char ignore-blanks)))

  (defun prev-char-is (x &optional ignore-blanks)
    (equal x (get-prev-char ignore-blanks))))

;; nav mode --------------------------------------------------



;; sending ---------------------------------------------------

(idefun send-sel (b)
        (comint-send-string b (substr+ret (mark) (point))))

(idefun send-line (b)
  (comint-send-string
   b
   (substr+ret
    (line-beginning-position 1)
    (line-beginning-position 2))))

;; term -----------------------------------------------------

(idefun term-launch (&optional name)
        (ansi-term "/bin/zsh" name))

(idefun term-reset (name)
  (let ((cb (current-buffer))
        (tbn (concat "*" name "*")))
    (select-window (get-buffer-window tbn))
    (kill-process)
    (kill-buffer tbn)
    (term-launch name)
    (select-window (get-buffer-window cb))))

(idefun term-paste ()
  (term-line-mode)
  (ns-paste-secondary)
  (term-char-mode))

(idefun term-init-local-keys ()
        (local-set-key (kbd "M-z") 'previous-multiframe-window)
        (local-set-key (kbd "s-v") 'term-paste))

;; red ------------------------------------------------------

(progn "red" 

  (idefun select-red-block ()
          (se-expand-max)
          (expand-region-to-enclosing-empty-lines))

  (idefun red-eval-top-lvl-block ()
          (select-red-block)
          (comint-send-string
           "*red*"
           (substr+ret (mark) (point)))
          (deactivate-mark))

  (idefun red-reload-file ()
    (save-buffer)
    (comint-send-string
     "*red*"
     (concat "do load %" (buffer-name) "\r")))

  (idefun red-launch ()
    (let (cb (current-buffer))
      (when (not (get-buffer "*red*"))
        (split-window-below)
        (windmove-down)
        (term-launch "red")
        (term-init-local-keys)
        (windmove-up)
        (comint-send-string "*red*" "red\r"))))

  (defvar red-current-buffer nil)

  (idefun red-repl-toggle-focus ()
    (if (equal "*red*" (buffer-name (current-buffer)))
        (if (not red-current-buffer)
            (other-window -1)
          (select-window (get-buffer-window red-current-buffer)))
      (progn
        (setq red-current-buffer (current-buffer))
        (select-window (get-buffer-window "*red*")))))

  (idefun red-relaunch ()
    (term-reset "red")
    (sit-for 1)
    (comint-send-string "*red*" "red\r")
    (red-repl-toggle-focus)
    (term-init-local-keys)
    (red-repl-toggle-focus))

  (idefun red-print-doc ()
    (forward-sexp)
    (backward-sexp)
    (mark-sexp)
    (comint-send-string "*red*" (concat "? " (sels-get) "\r")))

  (add-hook 'red-mode-hook
            (ifn ()
                 (print "red-refreshed!")
                 (hs-minor-mode 1)
                 (defmks red-mode-map
                                        ;""
                   "M-f" (ifn () (insert "func[][\n\n]") (backward-char 5))
                   "M-h" 'red-print-doc
                   "M-z" 'red-repl-toggle-focus
                   "C-M-l" 'red-launch
                   "C-r" 'red-relaunch
                   "M-r" 'red-reload-file
                   "M-<" (ifn () (send-line "*red*"))
                   "C-<" (ifn () (send-sel "*red*"))
                   "s-<" 'red-eval-top-lvl-block
                   "M-s-≤" 'red-reload-file)))

  ;(run-hooks 'red-mode-hook)
  )

;; shen -----------------------------------------------------

(add-hook 'shen-mode-hook
          (ifn ()
               (hs-minor-mode 1)
               (defmks shen-mode-map
                 ;"M-h"
                 "M-z" 'switch-to-shen
                 "C-M-l" 'inferior-shen
                                        ;"C-r" 'red-relaunch
                 ";" (ifn () (insert "\\"))
                 "M-s-≤" (ifn () (save-buffer) (shen-load-file (buffer-name (current-buffer))))
                 "M-<" (ifn () (backward-up-list) (forward-sexp) (shen-eval-last-sexp))
                 "C-<" 'shen-eval-region
                 "s-<" 'shen-eval-defun)))

;; clojure --------------------------------------------------

(add-hook 'cider-mode-hook
          (ifn ()
               (hs-minor-mode 1)
               (defmks cider-mode-map
                                        ;"M-h"
                 "M-z" 'cider-mode-switch-to-repl
                 "C-M-l" 'cider-jack-in
                                        ;"C-r" 'red-relaunch
                 "M-s-≤" (ifn () (save-buffer) (cider-eval-buffer))
                 "M-<" (ifn ()
                            (save-excursion
                              (backward-up-list)
                              (set-mark (point))
                              (forward-sexp)
                              (cider-eval-region (mark) (point)))
                            (deactivate-mark))
                 "M-d" 'cider-doc
                 "C-<" 'cider-eval-region
                 "s-<" 'cider-eval-defun-at-point)))

;; scheme ---------------------------------------------------

(setq geiser-scheme-implementation '(chicken))
(setq geiser-racket-binary "racket")

(add-hook 'scheme-mode-hook
          (ifn ()
               (hs-minor-mode 1)
               (defmks scheme-mode-map
                                        ;"M-h"
                 "M-z" 'geiser-mode-switch-to-repl
                 "C-M-l" 'inferior-scheme
                                        ;"C-r" 'red-relaunch
                 "M-s-≤" (ifn () (save-buffer) (geiser-compile-current-buffer))
                 "M-<" (ifn ()
                            (save-excursion
                              (backward-up-list)
                              (set-mark (point))
                              (forward-sexp)
                              (geiser-eval-region (mark) (point)))
                            (deactivate-mark))
                 "M-d" 'geiser-doc-symbol-at-point
                 "C-<" 'geiser-eval-region
                 "s-<" 'geiser-eval-definition)))

;; dired ----------------------------------------------------

(add-hook 'dired-mode
          (ifn ()
               (defmks dired-mode-map
                 "=" 'mhj/dwim-toggle-or-open
                 "<left>"
                 (ifn ()
                      (let ((c (current-buffer)))
                        (dired-up-directory)
                        (kill-buffer c)))
                 "<right>"
                 (ifn ()
                      (let ((c (current-buffer)))
                        (dired-find-file)
                        (kill-buffer c))))))

;; read only ------------------------------------------------

(idefun erase-read-only-buffer (b)
        (let ((cw (get-buffer-window (current-buffer))))
          (select-window (get-buffer-window b))
          (read-only-mode -1)
          (erase-buffer)
          (read-only-mode 1)
          (select-window cw)))

(idefun erase-messages ()
        (erase-read-only-buffer "*Messages*"))

;; marked state ---------------------------------------------

(idefun restore-arrow-bindings ()
  (defks
    "<left>" 'left-char
    "<right>" 'right-char
    "<up>" 'previous-line
    "<down>" 'next-line)) 

;; sexp -----------------------------------------------------

(progn "sexps"

       ;; TODO for better nav
       (idefun se-first? ())
       (idefun se-last? ())
       (idefun se-atom? ())
       (idefun se-beg? ())
       (idefun se-end? ())

       (idefun pb-open-square ()
         (insert "[]") (backward-char)
         ;(save-excursion (backward-char) (backward-delete-char 1))
         )

       (idefun pb-open-curly ()
         (paredit-open-curly)
         ;(save-excursion (backward-char) (backward-delete-char 1))
         )

       (idefun pb-open-round ()
         (paredit-open-round)
         ;(save-excursion (backward-char) (backward-delete-char 1))
         )

       (defun eval-current-sexp (x)
         (interactive "P")
         (print "here")
         (save-excursion
           (goto-char (sp-get (sp-get-enclosing-sexp) :end))
           (eval-last-sexp x))
         (deactivate-mark))

       (idefun kill-current-sexp ()
         (backward-up-list) (sp-kill-sexp))

       (idefun wrap-current-sexp ()
               (interactive)
               (sp-backward-up-sexp)
               (sp-wrap-with-pair "("))

       (idefun copy-current-sexp ()
               (sp-backward-up-sexp)
               (sp-mark-sexp)
               (ns-copy-including-secondary))

       (idefun wrap-sel-or-sexp (open close)
               ;(dbg open close (sels-get))
               ;(dbg (point-on-space-or-newline?) (point-on-closing-delimiter?))
               (cond

                ((sels-get)
                 (kill-region (mark) (point))
                 (insert open)
                 (insert close)
                 (backward-char)
                 (yank))

                ((point-on-space-or-newline?)
                 (insert open)
                 (insert close)
                 (backward-char))

                ((point-on-closing-delimiter?)
                 (insert " ")
                 (insert open)
                 (insert close)
                 (backward-char))

                ('else
                 (insert open)
                 (insert close)
                 (backward-char)))))

;; search --------------------------------------------------

(isearch-forward-symbol-at-point)

;; main edition overides  ----------------------------------

(idefun pb-copy ()
        (when (not (sels-get))
          (goto-char (end-of-line))
          (set-mark (beginning-of-line)))
        (kill-ring-save (mark) (point)))

(idefun pb-kill ()
        (cond
         ((sels-get) (kill-region (mark) (point)))
         ((empty-line) (kill-line))
         ('else (sp-kill-sexp))))

(idefun pb-escape ()
        (cond
         (buffer-read-only (kill-buffer))
         ((not (sels-empty?)) (set-mark nil))
         ((sels-empty?) (mc/keyboard-quit))
         ('else (mc/keyboard-quit))))

(idefun pb-backspace ()
        
        (cond
         ((sels-get)
          (sp-kill-region (mark) (point)))
         ('else
          (paredit-backward-delete))

         ((point-after-opening-delimiter?)
          (backward-char)
          (sp-kill-sexp))
         ((point-before-closing-delimiter?)
          (backward-char))
         ('else (sp-backward-delete-char))))

(idefun pb-help ()
        (if-let ((s (sels-get)))
            (describe-function (intern s))
          (call-interactively 'describe-function)))

;; misc -----------------------------------------------------

(idefun toggle-maximize-window () 
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (set-register '_ (list (current-window-configuration)))
           (delete-other-windows))))

(idefun toggle-helm ()
        (if  (equal " *Minibuf-1*"  (buffer-name (current-buffer)))
            (helm-keyboard-quit)
          (call-interactively 'helm-M-x)))

(progn "cycle buffer"

       (defvar cycle-buffers-state t)

       (idefun cycle-buffers ()
               (interactive)
               (setq cycle-buffers-state
                     (not cycle-buffers-state))
               (if cycle-buffers-state
                   (previous-buffer)
                 (next-buffer))))

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;; bindings ------------------------------------------------

(add-to-list 'sp--lisp-modes 'red-mode)

(defks

  ;; chars
  
  "M-l" (ifn () (insert "λ"))
  "M-L" (ifn () (insert "|"))
  "M-/" (ifn () (insert "\\"))
  "M-n" (ifn () (insert "~"))
  "$" (ifn () (paredit-open-square) (save-excursion (backward-char) (backward-delete-char)))
  "*" 'paredit-open-curly
  "\"" 'paredit-doublequote
  ;; delimiters

  "(" 'pb-open-round 
  "s-(" 'pb-open-square
  "M-(" 'pb-open-curly

  ;; windows

  "C-@" 'delete-window
  "C-&" 'toggle-maximize-window
  "C-é" 'split-window-right-and-focus
  "C-\"" 'split-window-below-and-focus
  "C-<tab>" 'other-window

  ;; cursors

  "C-S-<down>" 'mc/mark-next-like-this
  "C-S-<up>" 'mc/mark-previous-like-this
  "C-S-<left>" 'mc/mark-previous-like-this-word
  "C-S-<right>" 'mc/mark-next-like-this-word

  ;; general

  "s-D" 'dired-jump
  "s-h" 'pb-help
  "s-g" 'evil-goto-definition
  "s-l" 'helm-buffers-list
  "s-o" 'helm-find-files
  "s-f" 'spacemacs/helm-buffers-smart-do-search-region-or-symbol
  "M-p" 'cycle-buffers
  "M-m ô" 'spacemacs/helm-project-smart-do-search
  "M-m M-$" 'spacemacs/helm-project-smart-do-search-region-or-symbol
  "s-:" 'neotree-toggle
  "s-;" 'toggle-helm
  "s-s" 'save-buffer
  "s-t" 'term-launch

  ;; edition

  "M-v" 'helm-show-kill-ring
  "s-c" 'pb-copy
  "s-w" 'pb-kill
  "<escape>" 'pb-escape
  "<backspace>" 'pb-backspace
  "s-<backspace>" 'hungry-delete-backward
  "s-S-<backspace>" 'hungry-delete-forward

  "M-s-¬" (ifn () (backward-up-list) (indent-sexp))
  "M-a" 'wrap-current-sexp
  "M-w" 'kill-current-sexp
  "M-c" 'copy-current-sexp
  "M-s" 'sp-splice-sexp

  "C-M-S-<right>" 'sp-forward-slurp-sexp
  "C-M-S-<left>" 'sp-forward-barf-sexp
  "C-M-S-<up>" 'sp-backward-barf-sexp
  "C-M-S-<down>" 'sp-backward-slurp-sexp

  ;; toggling

  "C-M-<tab>" 'hs-hide-all
  "M-<tab>" 'evil-toggle-fold

  ;; selection

  "s-d" 'isearch-forward-symbol-at-point

  ;; nav ----------------------------------------------

  "M-<up>" 'sp-up-sexp
  "M-<down>" 'sp-down-sexp
  "M-<right>" 'sp-forward-sexp
  "M-<left>" 'sp-backward-sexp

  ;; transient mark sp moves here
                                        ;"M-S-<right>"  'sp-forward-slurp-sexp
                                        ;"M-S-<left>" 'sp-backward-slurp-sexp
                                        ;"M-S-<up>" 'sp-forward-barf-sexp
                                        ;"M-S-<down>" 'sp-backward-barf-sexp

  "C-<up>" 'windmove-up
  "C-<down>" 'windmove-down
  "C-<right>" 'windmove-right
  "C-<left>" 'windmove-left

  "s-S-<right>" 'ns-next-frame
  "s-S-<left>" 'ns-prev-frame
  "M-s-<left>" 'beginning-of-line-text 
  "M-s-<right>" 'end-of-line

  "s-<right>" 'next-buffer
  "s-<left>" 'previous-buffer
  "s-<up>" 'beginning-of-buffer
  "s-<down>" 'end-of-buffer

  "s-<mouse-1>" 'evil-goto-definition

  ;; mark

  "s-m" 'helm-mark-ring
  "s-M" 'helm-global-mark-ring

  ;; eval

  "s-<" 'eval-defun
  "M-<" 'eval-current-sexp
  "C-<" (ifn () (eval-region (mark) (point)))
  "M-s-≤" (ifn ()
               (save-buffer)
               (call-interactively 'eval-buffer)))

(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "`" nil
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp--org-skip-markup ms mb me)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

;; TODO ----------------------------------------------------

;; inserting [] and curly is clumpsy
;; sexp nav should definitively be as is and not always put point at begining
;; in red mode opening square + ret should trigger tab stuff
;; kill should work backward not forward
;; code folding should be pulti lvl and easy (double click unfolding should be nice)
;; exit marked state is clumsy too... it jumps sometimes
