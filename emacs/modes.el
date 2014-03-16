(add-to-list 'load-path "~/.dotfiles/emacs/packages/auto-complete")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/auto-indent-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/rainbow-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/iedit")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/ido-vertical-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/smex")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/virtualenvwrapper")

;; Automatically close parenthesis.
(electric-pair-mode)

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

(require' ido-vertical-mode)
(ido-vertical-mode)

;; Display M-x commands in the buffer list like ido-mode.
(require 'smex)
(smex-initialize)

;; Auto completion.
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.dotfiles/emacs/packages/auto-complete/dict")

(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Automatically indent code.
(require 'auto-indent-mode)
(auto-indent-global-mode)
(add-to-list 'auto-indent-disabled-modes-list 'python-mode)

;; Edit multiple occurences of the same string at the same time.
(require 'iedit)

;; Mode for note taking.
(require 'org-install)
(setq org-startup-indented t)

;; Choose the correct mode for header files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; Set default TAB functionality in makefile-mode
(add-hook 'makefile-mode-hook
          (lambda ()
            (define-key makefile-mode-map [tab] 'indent-for-tab-command)))

;; CSS major mode.
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; Python mode
(setq python-indent-guess-indent-offset nil)
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent 4)
            (setq python-indent-offset 4)))

;; virtualenvwrapper mode.
(require 'virtualenvwrapper)
(venv-initialize-eshell)
(setq venv-location "/home/gary/.virtualenvs")
