(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "green1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "turquoise"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "green1"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "turquoise"))))
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    PACKAGE STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))

;; Apparently needed for the package auto-complete
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    USAGE STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; special : dead accent key
(load-library "iso-transl")

;; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140414/dict")
(ac-config-default)
(global-auto-complete-mode t)

;; better search and replace
(require 'anzu)
(global-anzu-mode 1)

;; search over linebreaks and tabs
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace t)
(setq search-whitespace-regexp "[ \t\r\n]+")

;; replaces tabs with 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Use delete-selection mode.
(delete-selection-mode t)

;; change the yes or no with a y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; backup files handling
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))   ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;Save the point position for every file, and restore it when that file is reloaded.
(use-package saveplace
   :init
   (setq-default save-place t)
   (setq save-place-forget-unreadable-files
         t
         save-place-skip-check-regexp
         "\\`/\\(?:cdrom\\|floppy\\|mnt\\|/[0-9]\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)"))

;; deletes all the spaces at the end of a line when saving
(add-hook 'prog-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; auto-completion on pairable things.
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;;(global-set-key (kbd "'") 'skeleton-pair-insert-maybe) not wanted in
;;elisp, ocaml ...

;; Fixing the fix below:
;; Prevents issue where you have to press backspace twice when
;; trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; Avoids hitting C-s again to wraps the search when Failing I-search
;; is triggered
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 OCAML STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start tuareg mode on ml files
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Mode majeur pour éditer du code Caml" t)
(autoload 'camldebug "camldebug" "Exécuter le débogueur Caml" t)

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)
(put 'upcase-region 'disabled nil)

;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")

;; Set utop as default toplevel in tuareg mode
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; customization of ocaml mode
(load "~/elisp/sig2fun")
(add-hook 'tuareg-mode-hook
          (lambda ()
            (global-set-key (kbd "<f9>") 'sig2funregion)))

;; now consider the '_' as part of a word
(add-hook 'merlin-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       AbSolute Mode                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/elisp/absolute")
(add-hook 'absolute-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Ilpml Mode                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/elisp/ilpml")
(setq auto-mode-alist (cons '("\\.ilpml\\w?" . ilpml-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       LATEX STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; now consider the '\' as part of a word
(add-hook 'tex-mode-hook #'(lambda () (modify-syntax-entry ?\\ "w")))

;; now consider the '_' as part of a word
(add-hook 'tex-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       EMACS STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; downplay keywords, increase colorizing of the variables
(use-package color-identifiers-mode
  :ensure t
  :init
    (add-hook 'emacs-lisp-mode-hook 'color-identifiers-mode)
    :diminish color-identifiers-mode)

;;reload .emacs
(defun reloademacs()
 "reload .emacs file"
 (interactive)
 (load-file "~/.emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    DISPLAY STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/elisp/display")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    KEY BINDINGS STUFF                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/elisp/keys")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       COMPILE STUFF                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; path to closest Makefile
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from
the current directory towards root.  This may not do the correct thing
in presence of links. If it does not find FILE, then it shall return
the name of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; should work with win32 emacs
    (file-name-directory
     (expand-file-name
      file
      (loop
       for d = default-directory then (expand-file-name ".." d)
       if (file-exists-p (expand-file-name file d))
       return d
       if (equal d root)
       return nil)))))

;; redefining compile command
(require 'compile)
(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "cd %s; make" (get-closest-pathname)))
            ))

;; jump to first error in code when compiling
(setq compilation-auto-jump-to-first-error t)

;; set compilation output to first error
(setq compilation-scroll-output 'first-error)

;; colors in compilation buffer
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Makefiles are picky about whitespaces at the end of lines
(add-hook 'makefile-mode-hook
          (lambda()
            (global-whitespace-mode t)
            (setq show-trailing-whitespace t)))

;; completion engine: super saiyan mode
(ido-mode 1)
