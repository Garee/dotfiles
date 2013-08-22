;; Replace yes/no with y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Set the default TAB width.
(setq tab-width 2)

;; Store all file autosaves and backups in one directory.
(setq backup-directory-alist `(("." . "~/.backups")))

;; Store auto-save files in the backup directory.
(defvar auto-save-folder "~/.backups/auto-save")
(setq auto-save-file-name-transforms `((".*" ,auto-save-folder t)))

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

;; Replace text with inserted while mark is active.
(delete-selection-mode 1)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-u" 'undo)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-o" 'other-window)

;; Display the directory if two buffers have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

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
