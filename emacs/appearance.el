(add-to-list 'load-path "~/.dotfiles/emacs/packages/color-theme")

;; Remove useless GUI components.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

;; Parenthesis matching
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

;; Set the cursor type to a horizontal bar.
(setq default-cursor-type 'hbar)

;; Set the default font and ensure it works across frames.
(setq default-frame-alist
      '((font . "Dejavu Sans Mono-10")
	(vertical-scroll-bars)
	(tool-bar-lines . 0)
	(left-fringe . 0)
	(right-fringe . 0)
	(menu-bar-lines . 0)))

;; Replace all occurences of 'lambda' with the greek symbol.
(add-hook 'emacs-lisp-mode-hook 'sm-greek-lambda)
(add-hook 'python-mode-hook 'sm-greek-lambda)

;; Set the theme.
(require 'color-theme)
(color-theme-initialize)
(color-theme-solarized-dark)


;; Customise the mode line.
(setq-default mode-line-format
	      (list
	       " "
	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face))
	       "%02l [%m]"
               '(:eval (if (equal erc-modified-channels-object "") " " ""))
               '(t erc-modified-channels-object)
	       "["
               '(:eval (if overwrite-mode "Ovr" "Ins"))
	       '(:eval (when (buffer-modified-p) (concat ":Mod")))
	       '(:eval (when buffer-read-only (concat ":RO")))
	       "] ["
	       '(:eval (format-time-string "%H:%M"))
	       "] %-"))
