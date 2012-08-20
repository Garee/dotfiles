(add-to-list 'load-path "~/.dotfiles/emacs/packages/imenu")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/auctex")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/autopair")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/yasnippet")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/workgroups")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/iedit")
(add-to-list 'load-path "~/.dotfiles/emacs/packages/js2")

;; Enable ido-mode for opening buffers.
(ido-mode)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(autoload 'idomenu "idomenu" nil t)

;; Display the directory if two buffers have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(load "auctex.el" nil t t)
(setq TeX-PDF-mode t)

(require 'autopair)
(add-hook 'term-mode-hook
	  #'(lambda ()
	      (setq autopair-dont-activate t)
	      (autopair-mode -1)))
(autopair-global-mode)
(setq autopair-autowrap t)

(require 'yasnippet)
(yas/global-mode 1)

(require 'workgroups)
(workgroups-mode 1)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented t)
(setq org-directory "~/org")
(setq org-agenda-files (quote ("~/org")))
(setq org-default-notes-file "~/org/refile.org")
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w@)" "|" "DONE(d!)")))
(setq org-tag-alist '(("University" . ?u) ("Work" . ?w) ("Home" . ?h) ("Computer" . ?l) ("Reading" . ?r)))
(setq org-log-done 'time)
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/refile.org")
	 "* TODO %?\n%U\n%a\n")
	("n" "Note" entry (file "~/org/refile.org")
	 "* %?\n%U\n%a\n")
	("i" "Idea" entry (file "~/org/refile.org")
	 "* %?\n%U\n%a\n")
	("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\n%U\n")))

(require 'iedit)

;; Enhance javascript mode with js2.
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-mirror-mode nil)

;; Enable spellchecking for .txt and .tex
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(tex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))
