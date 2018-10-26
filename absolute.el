(require 'generic-x) ;; we need this

(define-generic-mode
    'absolute-mode                           ;; name of the mode to create
  '("/*" "*/")                               ;; comments start with '/*' and end with '*/'
  '("init" "constraints"
    "real" "int")                            ;; some keywords
  '(("\\(?:cos\\|exp\\|s\\(?:in\\|qrt\\)\\)"
     . 'font-lock-function-name-face))        ;; function name displayed differently
  '("\\.abs$")                               ;; files for which to activate this mode
  nil                                        ;; other functions to call
  "A mode for AbSolute files"                ;; doc string for this mode
  )
