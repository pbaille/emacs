;; -*- lexical-binding: t -*-

;; horizontal scroll -----------------------------------------
(comment
 (global-set-key [wheel-right] 'smooth-scroll/scroll-left)
 (global-set-key [wheel-left] 'smooth-scroll/scroll-right)
 (setq hscroll-margin 0)

 (require smooth-scroll)
 (global-set-key (kbd "<mouse-7>") '(lambda ()
                                      (interactive)
                                      (smooth-scroll/scroll-left)))
 (global-set-key (kbd "<mouse-6>") '(lambda ()
                                      (interactive)
                                      (smooth-scroll/scroll-right))))

;; utils -----------------------------------------------------
(progn

  (defmacro def-iloop (self message &rest cases)
    "define a key loop"
    `(defun ,self (key)
       (interactive ,(concat "c" message))
       (cond
        ,@(seq-map
           (fn (x)
               (cons `(eq key (string-to-char ,(car x)))
                     (append (cdr x) (list `(call-interactively ',self)))))
           cases))))

  (defmacro mk-prefix (n &rest bs)
    `(progn
       (setq ,n (make-sparse-keymap))
       (defmks ,n ,@bs)
       (fset ',n ,n)))

  (defmacro prettify-replace (mode &rest clauses)
    "example:
     (prettify-replace
       (\"zert\" \"ze..\" font-lock-builtin-face)
       (\"azer\" \"az..\" font-lock-builtin-face))"
    `(let ((spec '(,@clauses)))
       (dolist (s spec)
         (let ((regexp (nth 0 s))
               (repl (nth 1 s))
               (face (nth 2 s)))
           (font-lock-add-keywords
            ,mode
            `((,regexp 0
                       (prog1 ',face
                         (add-text-properties (match-beginning 0) (match-end 0)
                                              '(display ,repl))))))))))

  (comment
   (macroexpand '(mk-prefix yop "a" 'pouet))
   (mk-prefix yop+ "a" 'pouet)
   (macroexpand-1
    '(prettify-replace
      'emacs-lisp-mode
      ("zert" "ze..")
      ("azer" "az.." font-lock-builtin-face)))))

;; personal menu ---------------------------------------------
(progn

  (setq pb-keymap (make-sparse-keymap))
  (defk "<f1>" pb-keymap)

  (defmks pb-keymap
    "TAB" 'fold+
    "b" 'buffers+
    "w" 'windows+
    "d" 'describe+
    "t" 'shell+
    "e" 'eval+)

  ;; folding ----------------

  (mk-prefix fold+
             "a" 'yafolding-toggle-all
             "e" 'yafolding-toggle-element
             )

  (comment
   (defhydra hydra-origami (:color red)
     "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
     ("o" origami-open-node)
     ("c" origami-close-node)
     ("n" origami-next-fold)
     ("p" origami-previous-fold)
     ("f" origami-forward-toggle-node)
     ("a" origami-toggle-all-nodes)))

  ;; buffers ----------------

  (mk-prefix buffers+
             "m" 'popwin:messages)

  ;; eval -------------------

  (mk-prefix eval+
             "c" 'peval-current
             "e" 'peval-enclosing)

  (progn

    (defun peval-current ()
      (interactive)
      (save-excursion
        (goto-char (sp-get (sp-get-thing) :end))
        (call-interactively 'pp-eval-last-sexp)))

    (defun peval-enclosing ()
      (interactive)
      (save-excursion
        (goto-char (sp-get (sp-get-enclosing-sexp) :end))
        (call-interactively 'pp-eval-last-sexp))))

  ;; windows ----------------

  (mk-prefix windows+
             "r" 'window-iresize
             )

  (progn

    (def-iloop window-iresize
      "press t to enlarge or e to shrink"
      ("i"
       (enlarge-window 1))
      ("k"
       (enlarge-window -1))
      ("j"
       (enlarge-window -1 t))
      ("l"
       (enlarge-window 1 t))
      ))
  )

;; pretty syms -----------------------------------------------
(progn

  (push 'display font-lock-extra-managed-props) ;; Allow removal of display property by font-lock
  (push 'composition font-lock-extra-managed-props)
  (setq prettify-symbols-unprettify-at-point t)
  (global-prettify-symbols-mode 1)

  ;; (global-prettify-symbols-mode 0)

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq prettify-symbols-alist
                    '(("lambda" . 955)
                      ("progn" . ">-" )
                      ("comment" . "#")
                      ("defun" . "ƒ")
                      ("defmacro" . "µ")
                      ("interactive" . "I")))))

  (prettify-replace
   'emacs-lisp-mode
   ("idefun" "ƒi" font-lock-keyword-face)
   ("azerty" "az.." font-lock-keyword-face)))

;; abbrevs ---------------------------------------------------
(comment

 (define-abbrev-table 'global-abbrev-table
   '(("alpha" "α")
     ("inf" "∞")
     ("->" "→")))

 (abbrev-mode 1)

 (<= 1 2))

;; Fira code -------------------------------------------------
(progn

  ;; www  \ue100     **   \ue101    ***  \ue102    **/  \ue103
  ;;  *>  \ue104     */   \ue105     \\  \ue106    \\\  \ue107
  ;;  {-  \ue108     []   \ue109     ::  \ue10a    :::  \ue10b
  ;;  :=  \ue10c     !!   \ue10d     !=  \ue10e    !==  \ue10f
  ;;  -}  \ue110     --   \ue111    ---  \ue112    -->  \ue113
  ;;  ->  \ue114    ->>   \ue115     -<  \ue116    -<<  \ue117
  ;;  -~  \ue118     #{   \ue119     #[  \ue11a     ##  \ue11b
  ;; ###  \ue11c   ####   \ue11d     #(  \ue11e     #?  \ue11f
  ;;  #_  \ue120    #_(   \ue121     .-  \ue122     .=  \ue123
  ;;  ..  \ue124    ..<   \ue125    ...  \ue126     ?=  \ue127
  ;;  ??  \ue128     ;;   \ue129     /*  \ue12a    /**  \ue12b
  ;;  /=  \ue12c    /==   \ue12d     />  \ue12e     //  \ue12f
  ;; ///  \ue130     &&   \ue131     ||  \ue132    ||=  \ue133
  ;;  |=  \ue134     |>   \ue135     ^=  \ue136     $>  \ue137
  ;;  ++  \ue138    +++   \ue139     +>  \ue13a     +>  \ue13a
  ;; =:=  \ue13b     ==   \ue13c    ===  \ue13d    ==>  \ue13e
  ;;  =>  \ue13f    =>>   \ue140     <=  \ue141    =<<  \ue142
  ;; =/=  \ue143     >-   \ue144     >=  \ue145    >=>  \ue146
  ;;  >>  \ue147    >>-   \ue148    >>=  \ue149    >>>  \ue14a
  ;;  <*  \ue14b    <*>   \ue14c     <|  \ue14d    <|>  \ue14e
  ;;  <$  \ue14f    <$>   \ue150   <!--  \ue151     <-  \ue152
  ;; <--  \ue153    <->   \ue154     <+  \ue155    <+>  \ue156
  ;;  <=  \ue157    <==   \ue158    <=>  \ue159    <=<  \ue15a
  ;;  <>  \ue15b     <<   \ue15c    <<-  \ue15d    <<=  \ue15e
  ;; <<<  \ue15f     <~   \ue160    <~~  \ue161     </  \ue162
  ;; </>  \ue163     ~@   \ue164     ~-  \ue165     ~=  \ue166
  ;;  ~>  \ue167     ~~   \ue168    ~~>  \ue169     %%  \ue16a
  ;;   x  \ue16b      :   \ue16c      +  \ue16d      *  \ue16f

  (defun fira-code-mode--make-alist (list)
    "Generate prettify-symbols alist from LIST."
    (let ((idx -1))
      (mapcar
       (lambda (s)
         (setq idx (1+ idx))
         (let* ((code (+ #Xe100 idx))
                (width (string-width s))
                (prefix ())
                (suffix '(?\s (Br . Br)))
                (n 1))
           (while (< n width)
             (setq prefix (append prefix '(?\s (Br . Bl))))
             (setq n (1+ n)))
           (cons s (append prefix suffix (list (decode-char 'ucs code))))))
       list)))

  (defconst fira-code-mode--ligatures
    '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
      "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
      "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
      "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
      ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
      "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
      "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
      "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
      ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
      "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
      "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
      "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
      "x" ":" "+" "+" "*"))

  (defvar fira-code-mode--old-prettify-alist)

  (defun fira-code-mode--enable ()
    "Enable Fira Code ligatures in current buffer."
    (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
    (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
    (prettify-symbols-mode t))

  (defun fira-code-mode--disable ()
    "Disable Fira Code ligatures in current buffer."
    (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
    (prettify-symbols-mode -1))

  (define-minor-mode fira-code-mode
    "Fira Code ligatures minor mode"
    :lighter " Fira Code"
    (setq-local prettify-symbols-unprettify-at-point 'right-edge)
    (if fira-code-mode
        (fira-code-mode--enable)
      (fira-code-mode--disable)))

  (defun fira-code-mode--setup ()
    "Setup Fira Code Symbols"
    (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

  (provide 'fira-code-mode)

  ;; (fira-code-mode--enable)

  )

;; colors ----------------------------------------------------
(setq pb-colors

      '((sienna-1  . "#9a8f8b")
        (sienna-2  . "#a38b82")
        (sienna-3  . "#ab877a")
        (sienna-4  . "#b38372")
        (sienna-5  . "#bc7f69")
        (sienna-6  . "#c47b61")
        (sienna-7  . "#cc7759")
        (sienna-8  . "#d57350")
        (sienna-9  . "#dd6f48")
        (sienna-10 . "#e56b40")
        (sienna-11 . "#ee6737")
        (sienna-12 . "#f6632f")
        (sienna-13 . "#fe5f27")

        (sienna-1  . "#9a8f8b")
        (sienna-2  . "#a38b82")
        (sienna-3  . "#ab877a")
        (sienna-4  . "#b38372")
        (sienna-5  . "#bc7f69")
        (sienna-6  . "#c47b61")
        (sienna-7  . "#cc7759")
        (sienna-8  . "#d57350")
        (sienna-9  . "#dd6f48")
        (sienna-10 . "#e56b40")
        (sienna-11 . "#ee6737")
        (sienna-12 . "#f6632f")
        (sienna-13 . "#fe5f27")
        ))
