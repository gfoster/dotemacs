;;; last update: Thu Mar  8, 2012 09:35

;;; M-x list-colors-display  - show what all the colors look like in a buffer
;;; M-x list-faces-display   - show all defined faces and what they look like
;;; M-x color-themes-select  - show (and select from) all known themes in a buffer

;; Identify ourselves
(setq user-full-name    "Gary Foster"
      user-mail-address "gary.foster@gmail.com"
      mail-host-address '"gmail.com")

(global-font-lock-mode 1)

(add-to-list 'load-path "~/emacs.d")
(add-to-list 'load-path "~/emacs.d/lib")
(add-to-list 'load-path "~/emacs.d/lib/color-theme")
(add-to-list 'load-path "~/emacs.d/cedet-1.0/common")
(add-to-list 'load-path "~/emacs.d/ecb-2.40")
(add-to-list 'load-path "~/emacs.d/lib/rdebug-mode")
(add-to-list 'load-path "~/emacs.d/epg-0.0.16")
(add-to-list 'load-path "~/emacs.d/lib/yasnippet")

;; disable the bell on most (but not all) commands

(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(mwheel-scroll isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))

;; various requires
(require 'vc-git)
(require 'yasnippet)
;;(require 'git-blame)
(require 'auto-complete)
(require 'pulse)
(require 'comint)
(require 'crisp)
(require 'wiki-fu)
(require 'color-theme)
(require 'redo)
(require 'cucumber-mode)
(require 'dired+)
(require 'rdebug)
(require 'jira)
(require 'ido)
(require 'pretty-mode) ;; for shits
(require 'cedet)
;;(load-library "cedet")
(require 'ecb)
(require 'epa-setup)

;;;
(require 'logito)

(require 'gist)
(epa-file-enable)

;; setup easy gpg
;; any files that end in .gpg will trigger encryption mode automatically.
;; you can avoid the prompts for keys by prefixing this:
;; -*- mode: org -*- -*- epa-file-encrypt-to: ("my_key_email@foo.org") -*-
;;           ^^^ <- modified for the type of mode you want, of course


(require 'gf-snippets)
;;(require 'setup-python)
(require 'setup-ruby)

;; additional autoloads

(autoload 'egg-status "egg" nil t)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.mark" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml" . html-mode))

;; make things pretty
(color-theme-initialize)
(color-theme-gary)

;; set up the emacs server
(server-start)

;; override the vc-git-annotate-command to not pass rev (which is fucking broken)
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" )))

;; auto-reindent lines after a yank (paste) operation
;; (defadvice yank (after indent-region activate)
;;   (let ((mark-even-if-inactive t))
;;     (indent-region (region-beginning) (region-end) nil)))

;; enable our various modes
(yas/global-mode 1)
(global-pretty-mode 1)
(ido-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(pulse-toggle-integration-advice 1) ;; pulse lines when we jump, etc
(setq-default crisp-mode 1) ;; have to do this now to force it globally
(setq-default crisp-override-meta-x nil)
(delete-selection-mode 1)

(put 'narrow-to-region 'disabled nil)

;; turn off scrollbars because they are fugly
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; org mode settings
(setq org-log-done t)
(setq org-agenda-files (file-expand-wildcards "~/Dropbox/org_stuff/*.org"))

;; hard tabs suck
(setq-default indent-tabs-mode 'nil)

(setq default-major-mode 'text-mode)

;; dired mode tweaks
(put 'dired-find-alternate-file 'disabled nil)

;;; random hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; trailing whitespace sucks

;; key mapping overrides

;; these are normally already set via cua but I prefer to
;; override the normal cut/copy/kill stuff with my crisp mappings

(global-set-key [(hyper c)] 'crisp-set-clipboard)
(global-set-key [(hyper x)] 'crisp-kill-region)
(global-set-key [(hyper v)] 'crisp-yank-clipboard)

;; these used to map the mac keystrokes, but are done by
;; default in aquamacs.  I'm leaving them in here just
;; in case I need them in the future

;; (global-set-key [(hyper a)] 'mark-whole-buffer)
;; (global-set-key [(hyper s)] 'save-buffer)
;; (global-set-key [(hyper l)] 'goto-line)
;; (global-set-key [(hyper o)] 'find-file)
;; (global-set-key [(hyper f)] 'isearch-forward)
;; (global-set-key [(hyper g)] 'isearch-repeat-forward)
;; (global-set-key [(hyper w)]
;;                 (lambda () (interactive) (kill-buffer (current-buffer))))

(global-set-key [(hyper meta left)]  'previous-buffer)
(global-set-key [(hyper meta right)] 'next-buffer)

;; override or extend some of the default crisp-mode keybindings

;; these are normally bound to f3 and f4 respectively, but for some reason
;; f3 and f4 are being swallowed on this mac

(define-key crisp-mode-map [(f13) (down)]  'split-window-vertically)
(define-key crisp-mode-map [(f13) (right)] 'split-window-horizontally)
(define-key crisp-mode-map [(f14)]         'delete-window)
(define-key crisp-mode-map [(control f14)] 'delete-other-windows)

;; our own local keybindings

(define-key trac-wiki-mode-map "\C-c\C-c" 'trac-wiki-preview-default)
(global-set-key [(control c) (\\)]        'comment-region)
(global-set-key [(control c) (shift \\)]  'uncomment-region)
(global-set-key [(control c) (l)]         'ruby-lint)
(global-set-key [(control c) (control d)] 'gf-insert-datestamp)
(global-set-key [(control c) (control t)] 'gf-insert-timestamp)
(global-set-key [(control c) (g)]         'gf-send-private-gist)
(global-set-key [(control c) (b)]         'gf-insert-breakpoint)
(global-set-key [(control c) (a)]         'align-regexp)
(global-set-key [(control c) (s)]         'svn-status)
(global-set-key [(control tab)]           'yas/expand)
(global-set-key [(control c) (>)]         'indent-region)

;; love these two defuns so let's make C-e and C-a inherit them too
(global-set-key [(control e)] 'crisp-end)
(global-set-key [(control a)] 'crisp-home)

;; (global-set-key [(meta up)]   'gf-move-line-up)
;; (global-set-key [(meta down)] 'gf-move-line-down)

(global-set-key [(meta up)]   'move-text-up)
(global-set-key [(meta down)] 'move-text-down)

;;(global-set-key [(control c) (g)] 'egg-status)
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper shift z)] 'redo)

(define-key ctl-x-map   "d" 'diredp-dired-files)
(define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)

;; override comint
(define-key comint-mode-map (kbd "M-n") 'comint-next-input)
(define-key comint-mode-map (kbd "M-p") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

;; disable C-z iconification which drives me batshit
(when window-system
  (global-unset-key [(control z)]))   ; iconify-or-deiconify-frame

;; CEDET stuff

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; set up ecb

(setq ecb-ping-options '("-c" "1" "HOST"))

;; disable querying the external GUI for the key and do it within emacs
(setenv "GPG_AGENT_INFO" nil)

;; And finally Emacs custom settings.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-dwim t)
 '(case-fold-search t)
 '(compilation-scroll-output t)
 '(crisp-mode t nil (crisp))
 '(crisp-override-meta-x nil)
 '(cua-mode nil nil (cua-base))
 '(current-language-environment "UTF-8")
 '(cursor-type (quote box))
 '(custom-file nil)
 '(default-input-method "latin-1-prefix")
 '(ecb-clear-cachees-before-activate (quote t))
 '(ecb-clear-caches-before-activate t)
 '(ecb-directories-update-speedbar t)
 '(ecb-layout-name "left3")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-source-path (quote ("~/projects" ("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-buffer-style (quote ascii-guides))
 '(ecb-vc-enable-support t)
 '(ecb-vc-supported-backends (quote ((ecb-vc-dir-managed-by-SVN . ecb-vc-state) (ecb-vc-dir-managed-by-GIT . ecb-vc-state))))
 '(egg-enable-tooltip t)
 '(egg-mode-key-prefix "C-c v")
 '(egg-refresh-index-in-backround t)
 '(flymake-gui-warnings-enabled nil)
 '(flymake-mode nil t)
 '(flymake-start-syntax-check-on-find-file t)
 '(flymake-start-syntax-check-on-newline nil)
 '(global-auto-complete-mode t)
 '(ido-enable-regexp t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(jira-url "https://jira.talksum.com/rpc/xmlrpc")
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))
 '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
 '(org-startup-folded nil)
 '(pc-selection-mode t)
 '(pulse-iterations 20)
 '(py-electric-comment-p nil)
 '(python-honour-comment-indentation t)
 '(rdebug-restore-original-window-configuration t)
 '(ruby-indent-level 2)
 '(tab-width 4)
 '(tabbar-mode t nil (tabbar))
 '(tags-revert-without-query t)
 '(tags-table-list (quote ("/Users/gfoster/projects/talksum/CAMEL/TAGS")))
 '(tool-bar-mode nil)
 '(tramp-default-method "scpx")
 '(tramp-encoding-shell "/bin/bash")
 '(tramp-verbose 4)
 '(vc-handled-backends (quote (Git RCS SVN Bzr Hg Arch)))
 '(yaml-indent-offset 4)
 '(yas/snippet-dirs (quote ("/Users/gfoster/emacs.d/lib/yasnippet/snippets")) nil (yasnippet)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Monaco")))))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;(when
;    (load
;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;  (package-initialize))
