(load "~/Code/Emacs/elisp/utils.el")

(defks 
  "C-M-c" 'idris-case-split
  "C-M-d" 'idris-add-clause
  "C-M-p" 'idris-proof-search
  "C-M-l" 'idris-load-file
  "C-M-r" 'idris-refine
  "C-M-t" 'idris-type-at-point
  "C-M-s" 'idris-type-search
  "C-M-e" 'idris-make-lemma
  "C-<" 'idris-pop-to-repl)



