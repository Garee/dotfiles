(add-to-list 'load-path "~/.dotfiles/emacs/packages/imenu")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/auctex")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/autopair")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/yasnippet")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/iedit")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/js2")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/multi-web-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/haskell-mode")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/auto-complete")

;; Enable ido-mode for opening buffers.
(ido-mode)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(autoload 'idomenu "idomenu" nil t)

;; Display the directory if two buffers have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; auctex mode
(load "auctex.el" nil t t)
(setq TeX-PDF-mode t)

;; autopair mode
(require 'autopair)
(add-hook 'term-mode-hook
	  #'(lambda ()
	      (setq autopair-dont-activate t)
	      (autopair-mode -1)))
(autopair-global-mode)
(setq autopair-autowrap t)

;; yasnippet mode
(require 'yasnippet)
(yas/global-mode 1)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.dotfiles/emacs/packages/auto-complete/dict")
(ac-config-default)
(setq ac-auto-show-menu t)
(setq ac-ignore-case nil)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(add-to-list 'ac-modes 'html-mode)
(defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
  (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
  (add-to-list 'ac-sources 'ac-source-yasnippet))

;; org mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented t)
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files (quote ("~/Dropbox/org")))
(setq org-default-notes-file "~/Dropbox/org/refile.org")
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w@)" "|" "DONE(d!)")))
(setq org-tag-alist '(("University" . ?u) ("Work" . ?w) ("Home" . ?h) ("Computer" . ?l) ("Reading" . ?r)))
(setq org-log-done 'time)
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Dropbox/org/refile.org")
	 "* TODO %?\n%U\n%a\n")
	("n" "Note" entry (file "~/Dropbox/org/refile.org")
	 "* %?\n%U\n%a\n")
	("i" "Idea" entry (file "~/Dropbox/org/refile.org")
	 "* %?\n%U\n%a\n")
	("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
	 "* %?\n%U\n")))

;; iedit mode
(require 'iedit)

;; js2 mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-mirror-mode nil)

;; Multi-web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (ruby-mode "<%==\\|<%=\\|<%#\\|<%" "%>")
                  (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "erb"))
(multi-web-global-mode 1)

;; Enable spellchecking for .txt and .tex
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(tex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 0))))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Choose the correct mode for header files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; Switch to c-mode when working on an OpenCL kernel buffer.
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode))

;; haskell-mode configuration
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; CSS
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; Ruby on Rails
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
