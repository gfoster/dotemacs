;;; M-x list-colors-display  - show what all the colors look like in a buffer
;;; M-x list-faces-display   - show all defined faces and what they look like
;;; M-x color-themes-select  - show (and select from) all known themes in a buffer

;; Identify ourselves
(setq user-full-name    "Gary Foster"
      user-mail-address "gary.foster@gmail.com"
      mail-host-address '"gmail.com")

(setq load-path (append '("~/emacs.d" "~/emacs.d/lib" "~/emacs.d/cedet-1.0/common"
                          "~/emacs.d/ecb-2.40") load-path))

(add-to-list 'load-path "~/emacs.d/epg-0.0.16")

;; disable the bell on most (but not all) commands

(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(mwheel-scroll isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))

;; override the vc-git-annotate-command to not pass rev (which is fucking broken)

(require 'vc-git)
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" )))

;;(require 'git-blame)

;; byte compiler causes issues with ecb
;; so we don't use it by default anymore

;;(require 'bytecomp)
;;(setq byte-compile-verbose nil)
;;(setq byte-compile-warnings nil)
;;(require 'byte-code-cache)
;;(setq bcc-blacklist '("\\.emacs\\.history" "\\.emacs\\.desktop"))
;;(setq bcc-cache-directory "~/.emacs.d/elc")

(put 'narrow-to-region 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/mo-git-blame")

;; turn off scrollbars because they are fugly
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; nxhtml shit

;;(load "nxhtml/autostart.el")

;; additional autoloads

(autoload 'egg-status "egg" nil t)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.mark" . markdown-mode))

;; various requires

(require 'crisp)
(require 'wiki-fu)
(require 'color-theme)
(require 'redo)
(require 'ruby-electric)
(require 'cucumber-mode)
(require 'dired+)
(require 'rdebug)
(require 'jira)

;; load my personal utility crap
(load-library "gf.el")

(color-theme-initialize)
(color-theme-clarity)

;;; random hooks

;; trailing whitespace sucks

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max)))))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (define-key ruby-mode-map [(control return)] 'newline-and-indent)
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))

;;; MacOS X specific stuff

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; mac-key-mode is ok, but I prefer to map copy/kill/yank defuns to crisp
;; methods manually

;;(require 'mac-key-mode)

;; keyboard remapping to match Mac OSX shortcuts

(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper c)] 'crisp-set-clipboard)
(global-set-key [(hyper x)] 'crisp-kill-region)
(global-set-key [(hyper v)] 'crisp-yank-clipboard)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper o)] 'find-file)
(global-set-key [(hyper f)] 'isearch-forward)
(global-set-key [(hyper g)] 'isearch-repeat-forward)
(global-set-key [(hyper w)]
                (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key [(hyper .)] 'keyboard-quit)
(global-set-key [(hyper \])] 'indent-region)

;; our own local keybindings

(define-key trac-wiki-mode-map "\C-c\C-c" 'trac-wiki-preview-default)

;; override or extend some of the default crisp-mode keybindings

;; these are normally bound to f3 and f4 respectively, but for some reason
;; f3 and f4 are being swallowed on this mac

(define-key crisp-mode-map [(f13) (down)]    'split-window-vertically)
(define-key crisp-mode-map [(f13) (right)]   'split-window-horizontally)

(define-key crisp-mode-map [(f14)]           'delete-window)
(define-key crisp-mode-map [(control f4)]   'delete-other-windows)

;;(global-set-key [(control c) (g)] 'egg-status)
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper shift z)] 'redo)

(global-set-key "\C-cl" 'ruby-lint)
;;(global-set-key "\C-cl" 'org-store-link)
;;(global-set-key "\C-ca" 'org-agenda)

;; love these two defuns so let's make C-e and C-a inherit them too

(global-set-key [(control e)] 'crisp-end)
(global-set-key [(control a)] 'crisp-home)

(global-set-key [(control c) (control d)] 'gf-insert-datestamp)
(global-set-key [(control c) (control t)] 'gf-insert-timestamp)
(global-set-key [(meta up)]               'gf-move-line-up)
(global-set-key [(meta down)]             'gf-move-line-down)

;; pop up a man page on the command under (or at) point

(global-set-key [(hyper f1)]              'man-follow)

;; show this man page in a new frame instead of splitting the window

(setq Man-notify-method 'newframe)

;; disable C-z iconification which drives me batshit
(when window-system
  (global-unset-key [(control z)])) ; iconify-or-deiconify-frame

;; org mode settings

(setq org-log-done t)
(global-font-lock-mode 1)
(setq org-agenda-files (file-expand-wildcards "~/Dropbox/org_stuff/*.org"))

;; set up the emacs server
(server-start)

;; force our various modes

(column-number-mode 't)
(delete-selection-mode)
(show-paren-mode)
(delete-selection-mode)

;; hard tabs suck
(setq-default indent-tabs-mode 'nil)

(setq ruby-deep-indent-paren 'nil)
(delete-selection-mode)
(setq default-major-mode 'text-mode)

;; dired mode tweaks
(put 'dired-find-alternate-file 'disabled nil)
(define-key ctl-x-map   "d" 'diredp-dired-files)
(define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)

;; CEDET stuff

;; This is required by ECB which will be loaded later.
;; See cedet/common/cedet.info for configuration details.
(load-library "cedet")

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; set up ecb

;;(require 'ecb-autoloads)
(require 'ecb)
(setq ecb-ping-options '("-c" "1" "HOST"))

;; setup easy gpg
;; any files that end in .gpg will trigger encryption mode automatically.
;; you can avoid the prompts for keys by prefixing this:
;; -*- mode: org -*- -*- epa-file-encrypt-to: ("my_key_email@foo.org") -*-
;;           ^^^ <- modified for the type of mode you want, of course

(require 'epa-setup)
(epa-file-enable)

;; disable querying the external GUI for the key and do it within emacs
(setenv "GPG_AGENT_INFO" nil)

;; auto-reindent lines after a yank (paste) operation

(defadvice yank (after indent-region activate)
  (let ((mark-even-if-inactive t))
    (indent-region (region-beginning) (region-end) nil)))


;; And finally Emacs custom settings.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(compilation-scroll-output t)
 '(crisp-mode t nil (crisp))
 '(crisp-override-meta-x nil)
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(ecb-clear-cachees-before-activate (quote t))
 '(ecb-layout-name "left3")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-source-path (quote ("~/projects")))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-buffer-style (quote ascii-guides))
 '(egg-enable-tooltip t)
 '(egg-mode-key-prefix "C-c v")
 '(egg-refresh-index-in-backround t)
 '(flymake-gui-warnings-enabled nil)
 '(flymake-mode nil t)
 '(flymake-start-syntax-check-on-find-file t)
 '(flymake-start-syntax-check-on-newline nil)
 '(inhibit-startup-screen t)
 '(jira-url "https://jira.serv.io/rpc/xmlrpc")
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))
 '(org-startup-folded nil)
 '(rdebug-restore-original-window-configuration t)
 '(ruby-indent-level 4)
 '(tool-bar-mode nil)
 '(tramp-default-method "scpx")
 '(tramp-encoding-shell "/bin/bash")
 '(tramp-verbose 4)
 '(vc-handled-backends (quote (Git RCS CVS SVN SCCS Bzr Hg Arch MCVS)))
 '(yaml-indent-offset 4))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "grey75" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "apple-monaco"))))
 '(font-lock-comment-face ((t (:foreground "goldenrod"))))
 '(font-lock-keyword-face ((t (:foreground "darkorange"))))
 '(font-lock-string-face ((t (:foreground "seagreen"))))
 '(font-lock-variable-name-face ((t (:foreground "cornflowerblue"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil)))
 '(region ((t (:background "cornflowerblue" :foreground "black"))))
 '(region-face ((t (:foreground "black") (:background "cornflowerblue")))))
