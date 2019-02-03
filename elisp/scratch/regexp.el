(setq r1
  (rx (one-or-more (group "a"))
     "-"
     (or (group "b" eol)
         (group "ert"))))

(s-matches-p r1 "aaaa-b")

(format-time-string "%F")

(setq date-pattern
      (rx (group-n 3 (repeat 4 digit))
          "-"
          (group-n 2 (repeat 2 digit))
          "-"
          (group-n 1 (repeat 2 digit))))

(s-match-strings-all date-pattern
                     (format-time-string "%F"))

(defun rx-vec (&rest xs)
  (apply concat (append (cons "\[" xs) "\]")))

(rx (rx-vec "a" "b" "c"))

(s-matches-p ?w "aze")

(rx "(" ")")


