;; set autocompletion to ctrl + tab
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

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; customization of C-<RIGHT> (resp. C-<LEFT>) to make it switch
;; between forward-word (resp. backward-word) and forward-list
;; (resp. backward-list) according to the current (resp. previous) character
(defun forward-jump ()
  (interactive)
  (if (equal (get-char-code-property (char-after) 'general-category) 'Ps)
      (forward-list)
    (forward-word)))

(defun backward-jump()
  (interactive)
  (if (equal (get-char-code-property (char-before) 'general-category) 'Pe)
      (backward-list)
    (backward-word)))

(global-set-key  (kbd "C-<right>") 'forward-jump)
(global-set-key (kbd "C-<left>") 'backward-jump)

;; nice autoresize windows
(global-set-key [f9] 'balance-windows)

;; git status inside emacs
(global-set-key (kbd "C-x g") 'magit-status)
