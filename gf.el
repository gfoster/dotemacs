;; handy defuns and various crap
;; last updated Mon May 23, 2011 16:59

(defun gf-insert-timestamp ()
  "Insert the current time"
  (interactive "*")
  (insert (current-time-string)))

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