
;; faces -------------------------------------
(defmacro clj-defface (name parent)
  `(defface ,name
     ,(list 'quote (list (list t (list :inherit parent))))
     "clj face "))

(comment
 (macroexpand-1 '(clj-defface a font-lock-constant-face)))

(defmacro clj-deffaces (&rest pairs)
  `(progn
     ,@(seq-map
        (fn (pair) `(clj-defface ,(car pair) ,(cadr pair)))
        pairs)))

(clj-deffaces

 (clj-keyword-face             font-lock-builtin-face)
 (clj-nsprefix-face            nil)
 (clj-boolean-face             font-lock-constant-face)
 (clj-verb-face                nil)
 (clj-quotedsym-face           font-lock-builtin-face)
 (clj-char-face                font-lock-string-face)
 (clj-corekw-face              font-lock-keyword-face)
 (clj-fname-face               font-lock-function-name-face)
 (clj-varname-face             font-lock-variable-name-face)
 (clj-type-face                font-lock-type-face)
 (clj-lambdargs-face           font-lock-variable-name-face)
 (clj-var-face                 font-lock-type-face)
 (clj-regex-face               font-lock-type-face)
 (clj-dynvar-face              font-lock-variable-name-face)
 (clj-typehint-face            font-lock-builtin-face)
 (clj-string-face              font-lock-string-face)
 )


(defconst clj-faces

  (eval-when-compile

    `( ;; Top-level variable definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("def" "defonce"))
                ;; variable declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 'clj-corekw-face)
       (2 'clj-varname-face nil t))


      ;; quoted-symbols
      (;; "'\\<[a-z-]+"
       "'[a-z-]+"
       (0 'clj-quotedsym-face))


      ;;regex 
      (;"#\"<\\sw+\\\""
       ;;"#\"\\<[a-z-]+\""
       "#\"[^\"\\\"]*\""
       (0 'clj-regex-face))

      ("\"<\\sw+\\\"" (0 'clj-string-face))

      ;; Type definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defstruct" "deftype" "defprotocol"
                              "defrecord"))
                ;; type declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 'clj-corekw-face)
       (2 'clj-type-face nil t))


      ;; Function definition (anything that starts with def and is not
      ;; listed above)
      (,(concat "(\\(?:" clojure--sym-regexp "/\\)?"
                "\\(def[^ \r\n\t]*\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 'clj-corekw-face)
       (2 'clj-fname-face nil t)
       )

      
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       ;; (1 font-lock-keyword-face)
       ;; (2 font-lock-function-name-face nil t)
       (1 'clj-corekw-face)
       (2 'clj-fname-face nil t))


      ;; lambda arguments - %, %&, %1, %2, etc
      ("\\<%[&1-9]?"
       ;;(0 font-lock-variable-name-face)
       (0 'clj-lambdargs-face))


      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
            "recur" "throw" "try" "catch" "finally"
            "set!" "new" "."
            "monitor-enter" "monitor-exit" "quote") t)
         "\\>")
       1 'clj-corekw-face
       )


      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("letfn" "case" "cond" "cond->" "cond->>" "condp"
            "for" "when" "when-not" "when-let" "when-first" "when-some"
            "if-let" "if-not" "if-some"
            ".." "->" "->>" "as->" "doto" "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "ns" "in-ns"
            "with-open" "with-local-vars" "binding"
            "with-redefs" "with-redefs-fn"
            "declare") t)
         "\\>")
       1 'clj-corekw-face)


      ;; Macros similar to let, when, and while
      (,(rx symbol-start
            (or "let" "when" "while") "-"
            (1+ (or (syntax word) (syntax symbol)))
            symbol-end)
       0 'clj-corekw-face)
      (,(concat
         "\\<"
         (regexp-opt
          '("*1" "*2" "*3" "*agent*"
            "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
            "*command-line-args*" "*compile-files*"
            "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
            "*e" "*err*" "*file*" "*flush-on-newline*"
            "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
            "*print-dup*" "*print-length*" "*print-level*"
            "*print-meta*" "*print-readably*"
            "*read-eval*" "*source-path*"
            "*unchecked-math*"
            "*use-context-classloader*" "*warn-on-reflection*")
          t)
         "\\>")
       0 'clj-dynvar-face)


      ;; Dynamic variables - *something* or @*something*
      ("\\(?:\\<\\|/\\)@?\\(\\*[a-z-]*\\*\\)\\>"
       1 'clj-dynvar-face)


      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 'clj-boolean-face)


      ;; Character literals - \1, \a, \newline, \u0000
      ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)"
       0 'clj-char-face)


      ;; namespace definitions: (ns foo.bar)
      (,(concat "(\\<ns\\>[ \r\n\t]*"
                ;; Possibly metadata
                "\\(?:\\^?{[^}]+}[ \r\n\t]*\\)*"
                ;; namespace
                "\\(" clojure--sym-regexp "\\)")
       (1 'clj-type-face))


      ;; keywords: {:oneword/ve/yCom|pLex.stu-ff 0}
      (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
       (1 'clojure-keyword-face)
       ;; (2 font-lock-type-face)
       (2 'clj-keyword-face)
       (3 'clj-keyword-face)
       (4 'clj-keyword-face))
      (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--sym-regexp "\\)")
       (1 'clj-keyword-face)
       (2 'clj-keyword-face))


      ;; type-hints: #^oneword
      (,(concat "\\(#?\\^\\)\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
       (1 'default)
       (2 'clj-typehint-face)
       (3 'default)
       (4 'default))
      (,(concat "\\(#?\\^\\)\\(" clojure--sym-regexp "\\)")
       (1 'default)
       ;;(2 font-lock-type-face)
       (2 'clj-typehint-face)
       )


      ;; clojure symbols not matched by the previous regexps; influences CIDER's
      ;; dynamic syntax highlighting (CDSH). See https://git.io/vxEEA:
      (,(concat "\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
       (1 'clj-nsprefix-face
          ;;font-lock-type-face
          )
       ;; 2nd and 3th matching groups can be font-locked to `nil' or `default'.
       ;; CDSH seems to kick in only for functions and variables referenced w/o
       ;; writing their namespaces.
       (2 nil)
       (3 nil))

      (,(concat "\\(" clojure--sym-regexp "\\)")
       ;; this matching group must be font-locked to `nil' otherwise CDSH breaks.
       (1 nil))


      ;; #_ and (comment ...) macros.
      (clojure--search-comment-macro 1 clj-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "`")
       (1 'clj-comment-face prepend))
      ;; Highlight escaped characters in strings.
      (clojure-font-lock-escaped-chars 0 'bold prepend)
      ))

  "Default expressions to highlight in Clojure mode.")

;; theme -------------------------------------

(defmacro clj-theme-set-faces
  (&rest faces)
  `(custom-theme-set-faces
   'clj-theme
   ,@(seq-map
      (fn (spec)
          `'(,(car spec) ((t ,(cdr spec)))))
      faces)))

(comment

 (seq-map (fn (a) a) (list 1 2))

 (macroexpand
  '(clj-theme-set-faces
     (clj-keyword-face :foreground "#1d78a3")))

 (clj-theme-set-faces
  (clj-keyword-face :foreground "#1d78a3"))

 )

(clj-theme-set-faces
 (clj-keyword-face
  :foreground "#1d78a3")

 (clj-nsprefix-face
  :foreground "grey")

 (clj-boolean-face
  :foreground "#D446C3" :bold t)

 (clj-char-face
  :foreground "#BE1017")

 (clj-quotedsym-face
  :foreground "#D45D64")

 (clj-verb-face
  :foreground "#FAFAFA")

 (clj-quotedsym-face
  :foreground "#FAFAFA")

 (clj-corekw-face
  :foreground "#FAFAFA")

 (clj-fname-face
  :foreground "#FAFAFA")

 (clj-varname-face
  :foreground "#FAFAFA")

 (clj-type-face
  :foreground "#FAFAFA")

 (clj-lambdargs-face
  :foreground "#FAFAFA")

 (clj-var-face
  :foreground "#FAFAFA")

 (clj-regex-face
  :foreground "#FAFAFA")

 (clj-dynvar-face
  :foreground "#FAFAFA")

 (clj-typehint-face
  :foreground "#FAFAFA")

 (clj-string-face
  :foreground "#FAFAFA")
 )

(comment
 (let ((class '((class color) (min-colors 89))))

   (print class)

   (custom-theme-set-faces
    'clj-theme

                                        ; `(font-lock-custom-face               ((,class (:foreground "#ff0000"))))
    `(font-lock-type-face                 ((,class (:foreground "#6a3e6f"))))
    `(font-lock-string-face               ((,class (:foreground "#DF8F4E"))))
    `(font-lock-constant-face             ((,class (:foreground "#387485"))))
    `(font-lock-keyword-face              ((,class (:foreground "#31515a"))))
    `(font-lock-comment-face              ((,class (:foreground "#4c424d" :background "#292b2e"))))
    `(font-lock-function-name-face        ((,class (:foreground "#756C77"))))
    `(highlight-numbers-number            ((,class (:foreground "#c06534"))))
    `(font-lock-preprocessor-face         ((,class (:foreground "#ff8b4c"))))
    `(default                             ((,class (:foreground "#989a9a"))))
    `(trailing-whitespace                 ((,class (:background "#091f2e"))))
    `(font-lock-builtin-face              ((,class (:foreground "#1d78a3"))))

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
    `(org-level-2 ((,class (:foreground "#DA727E"))))
    `(org-level-3 ((,class (:foreground "#EF9C72"))))
    `(org-level-4 ((,class (:foreground "#685C79"))))
    `(org-level-5 ((,class (:foreground "#455C7B"))))

    `(org-todo ((,class (:foreground "#FF564D"))))
    `(org-meta-line ((,class (:foreground "#5C4379"))))
    `(org-link ((,class (:foreground "#8E66BA"))))
    `(org-special-keyword ((,class (:foreground "#6F5079"))))
    `(org-tag ((,class (:foreground "#05635E"))))


    ;; clojure

                                        ;`(clojure-keyword-face ((,class (:foreground "#1d78a3"))))
                                        ;`(clojure-nsprefix-face ((,class (:foreground "grey"))))
                                        ;`(clojure-boolean-face ((,class (:foreground "#D446C3" :bold t))))
                                        ;`(clojure-character-face ((,class (:foreground "#BE1017"))))
                                        ;`(clojure-quotedsym-face ((,class (:foreground "#D45D64"))))

    ;; should be replaced
                                        ;`(font-lock-function-name-face ((,class (:foreground "#EB6635"))))
                                        ;`(font-lock-variable-name-face ((,class (:foreground "#EB6635" :bold t))))
                                        ;`(font-lock-doc-face ((,class (:foreground "#656666"))))

                                        ;`(clojure-coremacro-face ((,class (:foreground "yellow"))))
                                        ;`(clojure-fname-face ((,class (:foreground "pink" :bold t))))
                                        ;`(clojure-varname-face ((,class (:foreground "pink" :bold t))))
                                        ;`(clojure-type-face ((,class (:foreground "red"))))
                                        ;`(clojure-lambdargs-face ((,class (:foreground "purple"))))

                                        ;`(highlight-numbers-number ((,class (:foreground "#DEAEC3"))))
                                        ;`(link ((,class (:foreground "#8E66BA"))))

    )))

(define-derived-mode clj-mode clojure-mode "clj-mode"
  "major mode for editing myhtml language code."
  (setq font-lock-defaults
        '(clj-faces    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . clojure-font-lock-syntactic-face-function))))

