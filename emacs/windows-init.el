(add-to-list 'load-path "~/dotfiles/emacs/packages/auto-complete")
(add-to-list 'load-path "~/dotfiles/emacs/packages/auto-indent-mode")
(add-to-list 'load-path "~/dotfiles/emacs/packages/rainbow-mode")
(add-to-list 'load-path "~/dotfiles/emacs/packages/iedit")
(add-to-list 'load-path "~/dotfiles/emacs/packages/ido-vertical-mode")
(add-to-list 'load-path "~/dotfiles/emacs/packages/smex")
(add-to-list 'load-path "~/dotfiles/emacs/packages/rainbow-delimiters")
(add-to-list 'load-path "~/dotfiles/emacs/packages/color-theme")

(defun toggle-fullscreen (&optional f)
  "Runs emacs in fullscreen with no window decoration."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

(defun sm-greek-lambda ()
  "Replace 'lambda' with the greek symbol."
  (font-lock-add-keywords nil `(("\\<lambda\\>"
				 (0 (progn (compose-region (match-beginning 0) (match-end 0)
							   ,(make-char 'greek-iso8859-7 107))
					   nil))))))

(defun c-c++-header ()
  "Sets the the appropriate mode for a header file."
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name ) 0 -1) "c")))
    (if (file-exists-p c-file)
	(c-mode)
      (c++-mode))))

(defvar electric-return-set "[\]}\)\"]")
(defun electric-return (arg)
  "If the text after the cursor matches `electic-return-set' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electric-return-set)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(setq default-directory "C:/Users/Gary Blackwood")

;; Replace yes/no with y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Set the default TAB width.
(setq tab-width 2)

;; Store all file autosaves and backups in one directory.
(setq backup-directory-alist `(("." . "~/backups")))

;; Store auto-save files in the backup directory.
(defvar auto-save-folder "~/backups/auto-save")
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Allow free use of the narrow commands.
(put 'narrow-to-region 'disabled nil)

;; Always line wrap.
(global-visual-line-mode t)

;; Replace text with inserted while mark is active.
(delete-selection-mode 1)

;; Disallow creation of newlines at end of buffer.
(setq next-line-add-newlines nil)
(setq mode-require-final-newline nil)

;; Run the garbage collector at 10mb (default is 0.7mb)
(setq gc-cons-threshold (* 10 1024 1024))

;; Automatically close parenthesis.
(electric-pair-mode)

;; Colourful parenthesis in scheme/lisp.
(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Return to the last visited place in a file.
(require 'saveplace)
(setq-default save-place t)

;; Display the directory if two buffers have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Highlight lines that go over an 80 column limit.
(add-hook 'prog-mode-hook 'whitespace-mode)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))

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
(add-to-list 'ac-dictionary-directories "~/dotfiles/emacs/packages/auto-complete/dict")

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
(add-hook 'python-mode-hook
          '(lambda ()
             (setq python-indent-guess-indent-offset nil)
             (setq python-indent 4)
             (setq python-indent-offset 4)))

(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-xm" 'smex)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-u" 'undo)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-xi" 'indent-buffer)
(global-set-key "\C-o" 'other-window)
(global-set-key (kbd "C-/") 'comment-dwim)

(global-set-key "\M-x" 'smex-major-mode-commands)
(global-set-key "\M-i" 'idomenu)
(global-set-key "\M-o" 'occur)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key "\M-;" 'iedit-mode-on-function)
(global-set-key "\M-r" 'replace-string)

(global-set-key (kbd "RET") 'electric-return)
(global-set-key [f11] 'toggle-fullscreen)

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

;; Set the default font and ensure it works across frames.
(setq default-frame-alist
      '((font . "Consolas-10")
	(vertical-scroll-bars)
	(tool-bar-lines . 0)
	(left-fringe . 0)
	(right-fringe . 0)
	(menu-bar-lines . 0)))

;; Replace all occurences of 'lambda' with the greek symbol.
(add-hook 'emacs-lisp-mode-hook 'sm-greek-lambda)
(add-hook 'python-mode-hook 'sm-greek-lambda)

;; Set the theme.
(require 'color-theme)
(color-theme-initialize)
(color-theme-solarized-dark)

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
