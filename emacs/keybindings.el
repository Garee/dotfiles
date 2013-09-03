(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-xm" 'smex)
(global-set-key "\M-x" 'smex-major-mode-commands)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-u" 'undo)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-i" 'indent-buffer)
(global-set-key "\C-o" 'other-window)
(global-set-key "\C-c" 'comment-dwim)

(global-set-key "\M-i" 'idomenu)
(global-set-key "\M-o" 'occur)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key "\M-;" 'iedit-mode-on-function)
(global-set-key "\M-r" 'replace-string)

(global-set-key (kbd "RET") 'electric-return)

(global-set-key [f11] 'toggle-fullscreen)
