;; full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; No scratch message
(setq initial-scratch-message "")

;; free from the hell of annoying buffers such like *Help*,
;; *Completions*, *compilation*, and etc.
(require 'popwin)
(popwin-mode 1)

;; title bar shows name of current buffer.
(setq frame-title-format '("emacs: %*%+ %b"))

;; colors customization. Works also with emacsclient
(defun colorize()
  (set-foreground-color "#DDCCAA")
  (set-background-color "#223355")
  ;; colorizing margins also
  (set-face-attribute 'fringe nil :background "#223355" :foreground "#DDCCAA")
  )
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
          (select-frame frame)
          (colorize))
        ))
(colorize)

;; No toolbars No scrollbars
(when (window-system)
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

;; line number, column number
(require 'linum)
(global-linum-mode 1)
(column-number-mode 1)
(line-number-mode 1)

;; Display color on color-code string (hex/rgb) directly. eg #FFF or red
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

;; closing parenthesis highlight
(load-library "paren")
(show-paren-mode 1)

;; Various keywords (in comments) are now flagged in a Red Error font
(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK|WARNING\\):" 1 font-lock-warning-face t)))))

;; colors stuff
(require 'ansi-color)

;; colors in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
