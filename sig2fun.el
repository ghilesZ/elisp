;; transforms a type to a skeleton to complete
;; ex:
;;   val min_binding_opt : 'a t -> (key * 'a) option
;; becomes
;;  let min_binding_opt (x1:'a t) : (key * 'a) option =
;;    (* TODO: replace the "failwith" with your own code *)
;;    failwith "function 'min_binding_opt' in file 'filename' not implemented"
;;  WARNING: the produced code might be dummy
;;  WARNING: does not handle yet arrow type argument

(setq counter 0)

;; dummy body of the function
;; we specify in the failwith msg the name of the function and the file wich
;; defines it
(defun body (fname)
  (setq com " =\n(* TODO: replace the \"failwith\" with your own code *)\n")
  (concat com "failwith \"value '" fname "' in file '"
          (file-name-nondirectory (buffer-file-name))
          "' not implemented\"\n"))

;; generation of argument name according to the type
(defun namegen (x)
  (setq counter (+ counter 1))
  (pcase x
    (`"unit" "()")
    (`"Format.formatter" "fmt")
    ((guard (string-match-p (regexp-quote "->") x)) (concat "f" (number-to-string counter)))
    (x
     (save-match-data
       (let* ((rgxspace (regexp-quote " "))
              (l (split-string x rgxspace));; in case type take argument ('a list, int option ...)
              (namelast (car (last l)));; we consider only the outer type constructor
              (firstchar (substring namelast 0 1)))
         (concat firstchar (number-to-string counter)))))))

;; (namegen "int")
;; (namegen "bool")
;; (namegen "Format.formatter")
;; (namegen "unit")
;; (namegen "'a list")
;; (namegen "'a list option")
;; (namegen "'a -> 'b")

;; in case there are tuples, we explode them
(defun handleatom(typ)
  (let((typregexp "['A-Za-z .]+")
       (nostar (replace-regexp-in-string "*" "," typ)))
    (concat
     "("
     (replace-regexp-in-string typregexp 'namegen nostar)
     ":"
     typ
     ") "
     )))

;; (handleatom "'a list")

;; we dont need to add an argument to the return type
(defun typatoms2arg(list)
  (setq counter 0)
  (let((returntyp (car (last list)))
       (argtyp (butlast list)))
    (concat
     (reduce 'concat (mapcar 'handleatom argtyp))
     ": "
     returntyp)))

;; removes only first occurence of "val "
(defun handlename(name)
  (replace-regexp-in-string "\\(val \\).*\\'" "" name nil nil 1))

(defun sig2fun(str)
  (setq counter 0) ;; reset the name counter
  (let*
      ((rgxcolon (regexp-quote ":"))
       (rgxarrow (regexp-quote "->"))
       (list (split-string str rgxcolon))
       (name (car list))
       (type (cadr list))
       (typatoms (split-string type rgxarrow))
       (trimed (mapcar 'string-trim typatoms))
       (name (string-trim (handlename name))))
    (concat "let " name " " (typatoms2arg trimed) (body name))))

;; few examples:
;; WORKING:
;; (sig2fun "val min_binding_opt : int -> float -> 'a list -> bool -> (key * 'a) option")
;; NOT WORKING:
;; (sig2fun "val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t")

;; integrating the function to buffers
(defun sig2funregion()
  (interactive)
  (let*((sig (buffer-substring-no-properties (region-beginning) (region-end)))
        (fun (sig2fun sig)))
    (delete-region (region-beginning) (region-end))
    (insert fun)
    (indent-for-tab-command)))
