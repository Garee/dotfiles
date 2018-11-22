; Initialize packages.
(package-initialize)

; Start in the home directory.
(setq default-directory "~/")

; Replace yes/no with y/n.
(fset 'yes-or-no-p 'y-or-n-p)

; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

; Store all file autosaves and backups in one directory.
(setq backup-directory-alist `(("." . "~/.backups")))

; Disable auto-save #files#.
(setq auto-save-default nil)

; Display the directory if two buffers have the same name.
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; Always follow symbolic links without a warning message.
(setq vc-follow-symlinks t)

; Auto reload files on change.
(global-auto-revert-mode t)

; Use cmd as meta rather than alt on mac.
(setq mac-command-modifier 'meta)

; Disallow creation of newlines at end of buffer.
(setq next-line-add-newlines nil)
(setq mode-require-final-newline nil)

; Rebind commonly used functions.
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-u") 'undo)

; Make eshell PATH the same as normal shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; Highlight matching parenthesis.
(show-paren-mode 1)

; Match parens. 
(electric-pair-mode 1)
(electric-indent-mode 1)

; Remove useless GUI components.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(menu-bar-mode 0)

; Set the default font.
(setq default-frame-alist
      '((font . "Hack-14")
        (vertical-scroll-bars)
        (tool-bar-lines . 0)
        (left-fringe . 0)
        (right-fringe . 0)
        (menu-bar-lines . 0)))

(custom-set-faces
 '(mode-line ((t (:family "Hack")))))
