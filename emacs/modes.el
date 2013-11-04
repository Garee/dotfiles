(add-to-list 'load-path "~/.dotfiles/emacs/packages/auto-indent-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/imenu")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/iedit")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/auctex")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/js2")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/multi-web-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/haskell-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/dash")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/s")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/smex")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/virtualenvwrapper")

;; Automatically close brackets.
(electric-pair-mode)

;; Return to last visited place in a file.
(require 'saveplace)
(setq-default save-place t)

;; Display the directory if two buffers have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Choose the correct mode for header files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; Switch to c-mode when working on an OpenCL kernel buffer.
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode))


;; Set default TAB functionality in makefile-mode
(add-hook 'makefile-mode-hook
          (lambda ()
            (define-key makefile-mode-map [tab] 'indent-for-tab-command)))

;; IRC configuration.
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; Display file/directory names in the buffer list.
(ido-mode)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(autoload 'idomenu "idomenu" nil t)

;; Display M-x commands in the buffer list like ido-mode.
(require 'smex)
(smex-initialize)

;; Automatically indent code
(require 'auto-indent-mode)
(auto-indent-global-mode)

;; Edit multiple occurences of the same string at the same time.
(require 'iedit)

;; LaTeX major mode.
(load "auctex.el" nil t t)

;; Mode for note taking.
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented t)

;; JavaScript major mode.
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)

;; Dynamically switch major mode in the same file depending on the current point location.
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (ruby-mode "<%==\\|<%=\\|<%#\\|<%" "%>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "erb"))
(multi-web-global-mode 1)

;; Haskell major mode.
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; CSS major mode.
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; Ruby major mode.
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; virtualenvwrapper mode.
(require 'virtualenvwrapper)
(venv-initialize-eshell)
(setq venv-location "/home/gary/.virtualenvs")
