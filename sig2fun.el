(defun body (fname)
  (setq com " =\n(* TODO: replace the \"failwith\" with your own code *)\n")
  (concat com "failwith \"function '" fname "' in file '"
          (file-name-nondirectory (buffer-file-name))
          "' not implemented\"\n"))

(defun namegen()
   (setq counter (+ counter 1))
   (concat "x" (number-to-string counter)))

(defun typatoms2arg(list)
  (setq counter 0)
  (let((returntyp (car (last list)))
       (argtyp (butlast list)))
    (concat
     (reduce 'concat
             (mapcar (function (lambda (x) (concat "(" (namegen) ":" x ") "))) argtyp))
     ": "
     returntyp)))

;; removes only first occurence of "val "
(defun handlename(name)
  (replace-regexp-in-string "\\(val \\).*\\'" "" name nil nil 1))

(defun sig2fun(str)
  (let*
      ((rgxcolon (regexp-quote ":"))
       (rgxarrow (regexp-quote "->"))
       (list (split-string str rgxcolon))
       (name (car list))
       (type (cadr list))
       (typatoms (split-string type rgxarrow))
       (trimed (mapcar (function (lambda (x) (string-trim x))) typatoms))
       (name (string-trim (handlename name))))
    (concat "let " name " " (typatoms2arg trimed) (body name))))

(defun sig2funregion()
  (interactive)
  (let*((sig (buffer-substring-no-properties (region-beginning) (region-end)))
        (fun (sig2fun sig)))
    (delete-region (region-beginning) (region-end))
    (insert fun)
    (indent-for-tab-command)))
