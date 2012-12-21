(add-to-list 'load-path "~/.dotfiles/emacs/packages/auto-indent-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/imenu")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/iedit")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/yasnippet")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/auctex")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/js2")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/multi-web-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/haskell-mode")

;; Enable ido-mode for opening buffers.
(ido-mode)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(autoload 'idomenu "idomenu" nil t)

;; electric pair mode
(electric-pair-mode)

;; auto indent mode
(require 'auto-indent-mode)
(auto-indent-global-mode)

;; Display the directory if two buffers have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; iedit mode
(require 'iedit)

;; yasnippet mode
(require 'yasnippet)
(yas-global-mode 1)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; auctex mode
(load "auctex.el" nil t t)

;; org mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented t)

;; js2 mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)

;; multi-web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (ruby-mode "<%==\\|<%=\\|<%#\\|<%" "%>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "erb"))
(multi-web-global-mode 1)

;; Choose the correct mode for header files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; Switch to c-mode when working on an OpenCL kernel buffer.
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode))

;; haskell-mode 
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; css mode
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; ruby mode
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
