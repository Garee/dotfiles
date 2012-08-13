;; Replace yes/no with y/n.
(fset 'yes-or-no-p 'y-or-n-p)

(setq c-basic-offset 2)

;; Store all file autosaves and backups in one directory.
(setq backup-directory-alist `(("." . "~/.backups")))

;; Disable auto-save #files#.
(setq auto-save-default nil)

;; Allow copy/paste outwith emacs.
(setq x-select-enable-clipboard t)

;; Always follow symbolic links without a warning message.
(setq vc-follow-symlinks t)

;; Don't display message in scratch buffer.
(setq initial-scratch-message nil)

;; Remove trailing whitespace.
(setq delete-trailing-whitespace t)

;; Allow free use of the narrow commands.
(put 'narrow-to-region 'disabled nil)

;; Always line wrap.
(global-visual-line-mode t)