(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "navajo white"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "sandy brown"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "indian red"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "navajo white"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "sandy brown"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "indian red"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "navajo white"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sandy brown"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "indian red"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red")))))

;;;;;;;;;;;;;;;;;;;;;;;;; END OF CUSTOM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    PACKAGE STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))

;; Apparently needed for the package auto-complete
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    DISPLAY STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; No scratch message
(setq initial-scratch-message "")

;; No toolbars No scrollbars
(when (window-system)
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

;; numeros de lignes, colonnes
(require 'linum)
(global-linum-mode 1)
(column-number-mode 1)
(line-number-mode 1)

;; colors
(set-foreground-color "#DDCCAA")
(set-background-color "#223355")

;; Display color on color-code string (hex/rgb) directly.
(require 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

;; Highlights parentheses, brackets, and braces according to their depth
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; 80 col rule
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(setq whitespace-space 'underline)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Recherche automatique des parenthèses ouvrantes et fermantes
(load-library "paren")
(show-paren-mode 1)

;; Various keywords (in comments) are now flagged in a Red Error font
(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    USAGE STUFF                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accent circonflexe entre autres
(load-library "iso-transl")

;; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140414/dict")
(ac-config-default)
(global-auto-complete-mode t)

;; best search and replace
(require 'anzu)
(global-anzu-mode 1)

;; remplace les tabs par 2 espaces
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

;;automatically reload .emacs after editing it
(defun reloademacs()
  "reload .emacs file whenever it is saved"
   (interactive)
   (load-file "~/.emacs")                   ;;reload .emacs
   )
(add-hook 'after-save-hook
          (lambda ()
            (when (equal buffer-file-name "/home/gg/.emacs")
              (reloademacs))))

;;Save the point position for every file, and restore it when that file is reloaded.
(use-package saveplace
   :init
   (setq-default save-place t)
   (setq save-place-forget-unreadable-files
         t
         save-place-skip-check-regexp
         "\\`/\\(?:cdrom\\|floppy\\|mnt\\|/[0-9]\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)"))

;; Supprime tous les espaces en fin de ligne lors de la sauvegarde
(add-hook 'prog-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

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

(load "~/gg/elisp/sig2fun")
(add-hook 'tuareg-mode-hook
          (lambda ()
            (global-set-key (kbd "<f9>") 'sig2funregion)))
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
 "reload .emacs file whenever it is saved"
 (interactive)
 (load-file "~/.emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    KEY BINDINGS STUFF                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; autocompletion a ctrl + tab
(global-set-key (kbd "C-<tab>") 'auto-complete)

;; kill current buffer a s-k
(global-set-key (kbd "s-k") 'kill-this-buffer)

;;repository tree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Symetrical of C-x o
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'switch-to-previous-buffer)

;; reload buffer with no confirmation, and reload .emacs
(defun revert-buffer-no-confirm ()
    "Reload .emacs file and revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)
    (reloademacs)
    ) ;;revert buffer
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       COMPILE STUFF                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from
the current directory towards root.  This may not do the correct thing
in presence of links. If it does not find FILE, then it shall return
the name of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; should work win win32 emacs
    (file-name-directory
     (expand-file-name
      file
      (loop
       for d = default-directory then (expand-file-name ".." d)
       if (file-exists-p (expand-file-name file d))
       return d
       if (equal d root)
       return nil)))))

(require 'compile)
(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "cd %s; make" (get-closest-pathname)))))