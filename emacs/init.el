;Configure package sources.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

; Start in the home directory.
(setq default-directory "~/")

; Replace yes/no with y/n.
(fset 'yes-or-no-p 'y-or-n-p)

; Automatically complete.
(ac-config-default)

; Enable snippets globally.
(yas-global-mode 1)

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

; Use cmd as meta rather than alt on mac.
(setq mac-command-modifier 'meta)

; Disallow creation of newlines at end of buffer.
(setq next-line-add-newlines nil)
(setq mode-require-final-newline nil)

; Rebind commonly used functions.
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-u") 'undo)

; Enable helm-mode globally.
(helm-mode 1)

; Sort helm results according to usage.
(helm-adaptive-mode 1)

; helm for searching.
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

; helm for describe-bindings
(global-set-key (kbd "C-h b")   'helm-descbinds)

; helm for switching buffers.
(global-set-key (kbd "C-x b")   'helm-mini)

; helm for managing the buffer list.
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

; helm for executing functions.
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)

; helm for opening files.
(global-set-key (kbd "C-x C-f") 'helm-find-files)

; Static analysis for code.
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(eval-after-load 'flycheck (flycheck-pos-tip-mode))

(add-hook 'after-init-hook 'global-flycheck-mode)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


; Make eshell PATH the same as normal shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Shell pop-up.
(setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
(global-set-key (kbd "C-c t") 'shell-pop)

(with-eval-after-load "esh-opt"
  (require 'virtualenvwrapper)
  (venv-initialize-eshell)
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

; Multiple cursors.
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; Undo tree
(global-undo-tree-mode)

; Highlight matching parenthesis.
(show-paren-mode 1)

; Match parenthesis automatically.
(smartparens-global-mode)

; avy search by char.
(avy-setup-default)
(global-set-key (kbd "C-c k") 'avy-goto-char)
(global-set-key (kbd "C-c l") 'avy-goto-line)

; Better popup windows.
(require 'popwin)
(popwin-mode 1)

; org mode.
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-html-postamble nil)
(setq org-html-htmlize-output-type 'css)
(setq org-agenda-files '("~/Org"))
(setq org-capture-templates
      '(("i" "Idea" entry (file+datetree "~/Org/ideas.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("n" "Note" entry (file+datetree "~/Org/notes.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("t" "Todo" entry (file+headline "~/Org/tasks.org")
         "* TODO %?\n  %i\n  %a")))
(global-set-key (kbd "C-c c") 'org-capture)

; Remove useless GUI components.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

; Set the default font.
(setq default-frame-alist
      '((font . "Monaco-14")
        (vertical-scroll-bars)
        (tool-bar-lines . 0)
        (left-fringe . 0)
        (right-fringe . 0)
        (menu-bar-lines . 0)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-color-mode-line-error-face ((t (:inherit flycheck-fringe-error :background "Red" :foreground "White" :weight normal))))
 '(flycheck-color-mode-line-info-face ((t (:inherit flycheck-fringe-info :background "Cyan" :foreground "White" :weight normal))))
 '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning :background "Orange" :foreground "White" :weight normal))))
 '(mode-line ((t (:family "Monaco")))))

; Custom mode line.
(powerline-default-theme)

; Enable the spacegray color theme.
(load-theme 'spacegray t)

; Fullscreen.
(toggle-frame-fullscreen)
