;;; ilpml-mode.el --- sample major mode for editing ilpml files. -*- coding: utf-8; lexical-binding: t; -*-

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq ilpml-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("let" "in" "and" "do" "if" "then" "else"
                          "while" "do" "try" "catch" "finally"
                          "lambda" "class" "method" "extends" "var"
                          "super" "self" "new" "var"))
            (x-constants '("pi"))
            (x-functions '("print" "newline"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-constants-regexp . font-lock-constant-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode ilpml-mode javascript-mode "ilpml mode"
  "Major mode for editing ilpml …"
  ;; code for syntax highlighting
  (setq font-lock-defaults '((ilpml-font-lock-keywords))))

;; Note: in the above, we based our mode on javascript-mode, because
;; the constructions are similar. It saves time in coding many
;; features, such as handling comment and indentation.

;; add the mode to the `features' list
(provide 'ilpml-mode)

;;; ilpml-mode.el ends here
