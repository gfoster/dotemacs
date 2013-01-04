;; handy defuns and various crap
;; last updated Wed Nov 30, 2011 10:29

(require 'cl)

(defun gf-insert-timestamp ()
  "Insert the current time"
  (interactive "*")
  (insert (current-time-string)))

(defun gf-insert-breakpoint ()
  "insert a language appropriate breakpoint"
  (interactive "*")
  (case major-mode

    ('python-mode
     (newline-and-indent)
     (insert "import pdb; pdb.set_trace()\n"))
    ('ruby-mode
     (newline-and-indent)
     (insert "debugger; 1\n"))
    (otherwise
     (gf-growl-chat "Emacs Notification" (format "no breakpoint appropriate for %s" major-mode)))
    ))

(defun gf-send-private-gist (&optional b e)
  "send a private gist"
  (interactive "r")

  (setq type (case major-mode
    ('emacs-lisp-mode 'el)
    ('ruby-mode       'rb)
    ('python-mode     'py)
    (otherwise 'nil)))

  (if type
      (setq filename (format "/tmp/gist.private.%s" type))
    (setq filename "/tmp/gist.private"))

  (if (use-region-p)
      (shell-command-on-region b e (format "cat > %s" filename))
    (shell-command-on-region (point-min) (point-max) (format "cat > %s" filename)))

  (shell-command
   (format "gist.py create --public=False %s" filename))

  (gf-growl-chat "Emacs Notification" (format "posted private gist at %s" (current-kill 0)))

  (shell-command
   (format "rm %s" filename)))

(defun gf-growl-chat (title message &optional sticky)
  (interactive "sTitle: \nsGrowl: ")
  (shell-command
   (format "growlnotify %s -m '%s' --appIcon 'Aquamacs' %s" title message (if sticky "--sticky" ""))))

;; Sticky notifications
(defun gf-growl-chat-sticky (title message)
  (interactive "sTitle: \nsGrowl: ")
  (gf-growl-chat title message t))

;; this is how you call it
(defun gf-growl-test ()
  (interactive "*")
  (gf-growl-chat "GF-growl-test" (format "Your major mode is %s" major-mode)))

(defun gf-insert-datestamp ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %H:%M")))

(defun gf-move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun gf-move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (gf-move-line (if (null n) -1 (- n))))

(defun gf-move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (gf-move-line (if (null n) 1 n)))

(defun ruby-lint ()
  "Performs a very basic Ruby lint-check on the current file."
  (interactive)
  (save-buffer)
  (shell-command (concat "ruby -c " (buffer-file-name))))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (gf-growl-chat "Emacs Notification" "Refreshed all open files")
  (message "Refreshed all open files."))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))

(defun writeroom ()
  "Switches to a WriteRoom-like fullscreen style"
  (interactive)
  (when (featurep 'aquamacs)
    ;; switch to white on black
    ;;(color-theme-initialize)
   ;; (color-theme-clarity)
    ;; switch to Garamond 36pt
    (aquamacs-autoface-mode 0)
    ;;(set-frame-font "-apple-garamond-medium-r-normal--36-360-72-72-m-360-iso10646-1")
    ;; switch to fullscreen mode
    (aquamacs-toggle-full-frame)))

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
        (exchange-point-and-mark))
     (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(provide 'gf-snippets)