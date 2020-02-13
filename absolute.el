(require 'generic-x) ;; you will need this

(define-generic-mode 'absolute-mode          ;; name of the mode to create
  '("/*" "*/")                               ;; comments: '/* some comment */'
  '("init" "constraints" "solutions"
    "real" "int" "in")                       ;; keywords
  '(("\\(?:cos\\|exp\\|s\\(?:in\\|qrt\\)\\)"
     . 'font-lock-function-name-face))       ;; function name
  '("\\.abs$")                               ;; Absolute problem description
  nil                                        ;; other functions to call
  "A mode for AbSolute files"                ;; doc string
  )
