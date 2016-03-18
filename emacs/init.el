;; Load any third-party libraries.
(add-to-list 'load-path "~/.dotfiles/emacs/packages/smex")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/ido-vertical-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/markdown-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/exec-path-from-shell")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/js2-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/web-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/auto-complete")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/yasnippet")

;; Configure package sources.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Start in the home directory.
(setq default-directory "~")

;; Replace yes/no with y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Set the default TAB width.
(setq tab-width 4)
(defvar c-basic-offset 4)

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

;; Remove trailing whitespace on save.
(defvar delete-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Always line wrap.
(global-visual-line-mode t)

;; Replace text with inserted while mark is active.
(delete-selection-mode 1)

;; Disallow creation of newlines at end of buffer.
(setq next-line-add-newlines nil)
(setq mode-require-final-newline nil)

;; Match parenthesis.
(electric-pair-mode 1)

;; Return to the last visited place in a file.
(require 'saveplace)
(setq-default save-place t)

;; Display the directory if two buffers have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Display file/directory names in the buffer list.
(ido-mode)
(defvar ido-enable-flex-matching t)
(defvar ido-everywhere t)
(autoload 'idomenu "idomenu" nil t)

;; Display ido mode vertically.
(require 'ido-vertical-mode)
(ido-vertical-mode)

;; Display M-x commands in the buffer list like ido-mode.
(require 'smex)
(smex-initialize)

;; Use markdown-mode for .md files.
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Mode for note taking.
(require 'org-install)
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-html-postamble nil)
(setq org-html-htmlize-output-type 'css)

(defun org-html-publish-dir()
  "Publish all org files in a directory"
  (interactive)
  (save-excursion
    (mapc
     (lambda (file)
       (with-current-buffer
       (find-file-noselect file)
     (org-html-export-to-html)))
       (file-expand-wildcards  "*.org"))))

;; Ensure environment variables are present within Emacs shell.
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Web dev mode for HTML templates.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; JavaScript mode.
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))

;; Snippets
(require 'yasnippet)
(yas-global-mode 1)


;; package-install auto-complete
(ac-config-default)

;; package-install flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Custom Keybindings.
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-o" 'other-window)
(global-set-key "\C-u" 'undo)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-/" 'hippie-expand)

;; Use cmd as meta rather than alt on mac.
(setq mac-command-modifier 'meta)

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
      '((font . "Monaco-14")
        (vertical-scroll-bars)
        (tool-bar-lines . 0)
        (left-fringe . 0)
        (right-fringe . 0)
        (menu-bar-lines . 0)))

;; Set the theme.
(if window-system
    (progn
        (add-to-list 'custom-theme-load-path "~/.dotfiles/emacs/themes")
        (load-theme 'spacegray t)))

;; Customise the mode line.
(custom-set-faces
 '(mode-line ((t (:family "Monaco")))))
(setq-default mode-line-format
              (list
               " "
               '(:eval (propertize "%b" 'face 'font-lock-keyword-face))
               " %l [%m] "
               '(:eval (if (equal erc-modified-channels-object "") " " ""))
               '(t erc-modified-channels-object)
               "["
               '(:eval (if overwrite-mode "Ovr" "Ins"))
               '(:eval (when (buffer-modified-p) (concat ":Mod")))
               '(:eval (when buffer-read-only (concat ":RO")))
               "] ["
               '(:eval (format-time-string "%H:%M"))
               "] %-"))
