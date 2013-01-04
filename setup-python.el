(require 'ipython)

(autoload 'python-mode "python-mode" "Python Mode." t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             ;;(set-variable 'py-indent-offset 4)
;;             ;;(set-variable 'py-smart-indentation nil)
;;             (set-variable 'indent-tabs-mode nil)
;;             (define-key py-mode-map (kbd "RET") 'newline-and-indent)
;;             (define-key py-mode-map [tab] 'indent-for-tab-command)
;;             (define-key py-mode-map (kbd "C-<tab>") 'yas/expand)
;;             (setq yas/after-exit-snippet-hook 'indent-according-to-mode)
;;             ;;(smart-operator-mode-on)
;;             ))

;; pymacs
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; Auto Syntax Error Hightlight
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (push '(?' . ?')
;;                     (getf autopair-extra-pairs :code))
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-python-triple-quote-action))))

;; setup pylookup

(setq pylookup-program "~/bin/pylookup.py")
(setq pylookup-db-file "~/bin/pylookup.db")
(global-set-key "\C-ch" 'pylookup-lookup)

;; setup anything.el

;; (require 'anything)
;; (require 'anything-ipython)

;; (require 'anything-ipython)
;; (add-hook 'python-mode-hook #'(lambda ()
;;                                 (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
;; (add-hook 'ipython-shell-hook #'(lambda ()
;;                                   (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))


;; (when (require 'anything-show-completion nil t)
;;   (use-anything-show-completion 'anything-ipython-complete
;;                                 '(length initial-pattern)))

(provide 'setup-python)
