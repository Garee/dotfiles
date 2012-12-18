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
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
	(c-mode)
      (c++-mode))))

(defun indent-all ()
  "Indents the entire buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))
