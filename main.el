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

;; remove annoying beep
(setq visible-bell       nil
      ring-bell-function #'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    PACKAGE STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; list the packages i want
(setq package-list '(anzu auto-complete neotree rainbow-mode))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; needed for some packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; very nice: install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    USAGE STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; special : dead accent key
(load-library "iso-transl")

;; better search and replace that shows the number of occurences
(require 'anzu)
(global-anzu-mode 1)

;; colum number
(setq colum-number-mode t)

;; auto-complete
(setq auto-complete-mode t)

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
(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq-default save-place t))

;; deletes all the spaces at the end of a line when saving
(add-hook 'prog-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; ignored extensions
(setq completion-ignored-extensions
    (append completion-ignored-extensions
        (quote
        (".exe" ".cmi" ".cmo" "cmx" ".o"))))

;; auto-completion on pairable things.
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;;(global-set-key (kbd "'") 'skeleton-pair-insert-maybe) not wanted in
;;elisp, ocaml ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 OCAML STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start tuareg mode on ml files
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Mode majeur pour éditer du code Caml" t)
(autoload 'camldebug "camldebug" "Exécuter le débogueur Caml" t)
;; Start tuareg menhir on mly files
(setq auto-mode-alist (cons '("\\.mly\\w?" . tuareg-menhir-mode) auto-mode-alist))

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(require 'merlin-ac)
(setq merlin-ac-setup 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)
(put 'upcase-region 'disabled nil)
;;jump to definition in .ml (not .mli)
(setq merlin-locate-preference 'ml)
;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")

;; Set utop as default toplevel in tuareg mode
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode t)

;; customization of ocaml mode
(load "~/elisp/sig2fun")
(add-hook 'tuareg-mode-hook
          (lambda ()
            (global-set-key (kbd "<f9>") 'sig2funregion)))

;; now consider the '_' as part of a word
(add-hook 'tuareg-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(require 'ocamlformat)
(add-hook 'tuareg-mode-hook (lambda ()
  (add-hook 'before-save-hook #'ocamlformat-before-save)))

;; autoformating on saving
(require 'ocamlformat)
(add-hook 'tuareg-mode-hook (lambda ()
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save)))

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

;; now consider the '_' as part of a word
(add-hook 'tex-mode-hook #'(lambda () ))

;; Display mini table of content in separate buffer with "ctrl+c ="
(add-hook 'tex-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       EMACS STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mail stuff
(setq user-mail-address "ghiles.ziat@gmail.com"
      user-full-name "Ghiles Ziat")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq send-mail-function		'smtpmail-send-it
      message-send-mail-function	'smtpmail-send-it
      smtpmail-smtp-server		"smtp.gmail.com")

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

(require 'cl)

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

;; I'm not scared of saving everything.
(setq compilation-ask-about-save nil)

;; jump to first error in code when compiling
(setq compilation-auto-jump-to-first-error t)

;; set compilation output to first error
(setq compilation-scroll-output 'first-error)

;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)

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

;; Open the file name being pointed in an other window or dired
(defun my-directory-or-file-p (path)
  "return t if path is a directory,
return nil if path is a file"
  (car (file-attributes path)))

(defun my-open-emacs-at-point ()
  "open the file with opening emacs"
  (interactive)
  (require 'ffap)
  (let ((file (or (ffap-url-at-point)
                  (ffap-file-at-point))))
    (unless (stringp file)
      (error"No file or URL found"))
    (when (file-exists-p (expand-file-name file))
      (setq file (expand-file-name file)))
    (message "Open: %s" file)

    (if (my-directory-or-file-p file)
      (dired-other-window file)
      (find-file-other-window file))
    ))

(global-set-key (kbd "\C-x C-g") 'my-open-emacs-at-point)
