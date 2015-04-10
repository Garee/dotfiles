(add-to-list 'load-path "~/.dotfiles/emacs/packages/ido-vertical-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/smex")

;; Start in the home directory.
(setq default-directory "~")

;; Replace yes/no with y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Set the default TAB width.
(setq tab-width 4)
(setq c-basic-offset 4)

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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Always line wrap.
(global-visual-line-mode t)

;; Replace text with inserted while mark is active.
(delete-selection-mode 1)

;; Disallow creation of newlines at end of buffer.
(setq next-line-add-newlines nil)
(setq mode-require-final-newline nil)

;; Return to the last visited place in a file.
(require 'saveplace)
(setq-default save-place t)

;; Display the directory if two buffers have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Display file/directory names in the buffer list.
(ido-mode)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(autoload 'idomenu "idomenu" nil t)

;; Display file choices vertically.
(require 'ido-vertical-mode)
(ido-vertical-mode)

;; Display M-x commands in the buffer list like ido-mode.
(require 'smex)
(smex-initialize)

;; Mode for note taking.
(require 'org-install)
(setq org-startup-indented t)

;; Choose the correct mode for header files.
(defun c-c++-header ()
  "Sets the the appropriate mode for a header file."
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name ) 0 -1) "c")))
    (if (file-exists-p c-file)
	(c-mode)
      (c++-mode))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; Custom Keybindings.
(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-xm" 'smex)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-u" 'undo)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-xi" 'indent-buffer)
(global-set-key "\C-o" 'other-window)

(global-set-key "\M-x" 'smex)
(global-set-key "\M-o" 'occur)
(global-set-key "\M-/" 'hippie-expand)

;; Remove useless GUI components.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

;; Parenthesis matching.
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

;; Set the cursor type to a horizontal bar.
(setq default-cursor-type 'hbar)

;; Set the default font.
(setq default-frame-alist
      '((font . "Consolas-10")
	(vertical-scroll-bars)
	(tool-bar-lines . 0)
	(left-fringe . 0)
	(right-fringe . 0)
	(menu-bar-lines . 0)))

;; Customise the mode line.
(custom-set-faces '(mode-line ((t (:family "Consolas")))))
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
