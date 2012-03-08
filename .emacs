;;; last update: Wed Feb 29, 2012 17:03

;;; M-x list-colors-display  - show what all the colors look like in a buffer
;;; M-x list-faces-display   - show all defined faces and what they look like
;;; M-x color-themes-select  - show (and select from) all known themes in a buffer

;; Identify ourselves
(setq user-full-name    "Gary Foster"
      user-mail-address "gary.foster@gmail.com"
      mail-host-address '"gmail.com")

(setq load-path (append '("~/emacs.d" "~/emacs.d/color-theme" "~/emacs.d/lib" "~/emacs.d/cedet-1.0/common"
                          "~/emacs.d/ecb-2.40") load-path))

(add-to-list 'load-path "~/emacs.d/epg-0.0.16")
(add-to-list 'load-path "~/emacs.d/yasnippet")

;; disable the bell on most (but not all) commands

(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(mwheel-scroll isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))

;; override the vc-git-annotate-command to not pass rev (which is fucking broken)

(require 'vc-git)
(defun vc-git-(and )nnotate-command (file buf &optional rev)
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


;; yassnippet

(require 'yasnippet)
(yas/global-mode 1)

;; python stuff

(require 'python-setup)

;; autocompletion

(require 'auto-complete)
(require 'pulse)
(pulse-toggle-integration-advice t)
;; additional autoloads

(autoload 'egg-status "egg" nil t)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

;;(autoload 'autopair-global-mode "autopair" nil t)
;;(autopair-global-mode)


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.mark" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml" . html-mode))

;; override comint
(require 'comint)
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

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
(require 'ido)
(require 'pretty-mode) ;; for shits

;; FORCE crisp-mode globally
(setq-default crisp-mode 't)
(setq-default crisp-override-meta-x nil)
(global-pretty-mode 1)

(ido-mode)

;; load my personal utility crap
(require 'gf-snippets)

(color-theme-initialize)
(color-theme-clarity)

;;; random hooks

;; trailing whitespace sucks

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      'untabify-buffer)
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

(global-set-key [(hyper \])] 'indent-region)

(global-set-key [(hyper meta left)] 'previous-buffer)
(global-set-key [(hyper meta right)] 'next-buffer)

;; override or extend some of the default crisp-mode keybindings

;; these are normally bound to f3 and f4 respectively, but for some reason
;; f3 and f4 are being swallowed on this mac

(define-key crisp-mode-map [(f13) (down)]    'split-window-vertically)
(define-key crisp-mode-map [(f13) (right)]   'split-window-horizontally)

(define-key crisp-mode-map [(f14)]           'delete-window)
(define-key crisp-mode-map [(control f4)]    'delete-other-windows)

;; our own local keybindings

(define-key trac-wiki-mode-map "\C-c\C-c" 'trac-wiki-preview-default)
(global-set-key [(control c) (\\)]        'comment-region)
(global-set-key [(control c) (shift \\)]  'uncomment-region)
(global-set-key [(control c) (l)]         'ruby-lint)
(global-set-key [(control c) (control d)] 'gf-insert-datestamp)
(global-set-key [(control c) (control t)] 'gf-insert-timestamp)
(global-set-key [(control c) (control b)] 'gf-insert-breakpoint)
(global-set-key [(control c) (a)]         'align-regexp)
(global-set-key [(control c) (s)]         'svn-status)

;; love these two defuns so let's make C-e and C-a inherit them too

(global-set-key [(control e)] 'crisp-end)
(global-set-key [(control a)] 'crisp-home)

(global-set-key [(meta up)]   'gf-move-line-up)
(global-set-key [(meta down)] 'gf-move-line-down)

;;(global-set-key [(control c) (g)] 'egg-status)
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper shift z)] 'redo)

;;(global-set-key "\C-cl" 'org-store-link)
;;(global-set-key "\C-ca" 'org-agenda)

;; pop up a man page on the command under (or at) point

(global-set-key [(hyper f1)] 'man-follow)

;; show this man page in a new frame instead of splitting the window

(setq Man-notify-method 'newframe)

;; disable C-z iconification which drives me batshit
(when window-system
  (global-unset-key [(control z)]))   ; iconify-or-deiconify-frame

(global-font-lock-mode 1)

;; org mode settings

(setq org-log-done t)
(setq org-agenda-files (file-expand-wildcards "~/Dropbox/org_stuff/*.org"))

;; set up the emacs server
(server-start)

;; force our various modes

(column-number-mode 't)
(delete-selection-mode 't)

(show-paren-mode 't)

;; hard tabs suck
(setq-default indent-tabs-mode 'nil)

(setq ruby-deep-indent-paren 'nil)
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
 '(ac-dwim t)
 '(case-fold-search t)
 '(compilation-scroll-output t)
 '(crisp-mode t nil (crisp))
 '(crisp-override-meta-x nil)
 '(current-language-environment "UTF-8")
 '(cursor-type (quote box))
 '(custom-file nil)
 '(default-input-method "latin-1-prefix")
 '(ecb-clear-cachees-before-activate (quote t))
 '(ecb-directories-update-speedbar t)
 '(ecb-layout-name "left3")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-source-path (quote ("~/projects")))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-buffer-style (quote ascii-guides))
 '(ecb-vc-supported-backends (quote ((ecb-vc-dir-managed-by-SVN . ecb-vc-state) (ecb-vc-dir-managed-by-GIT . ecb-vc-state))))
 '(egg-enable-tooltip t)
 '(egg-mode-key-prefix "C-c v")
 '(egg-refresh-index-in-backround t)
 '(flymake-gui-warnings-enabled nil)
 '(flymake-mode nil t)
 '(flymake-start-syntax-check-on-find-file t)
 '(flymake-start-syntax-check-on-newline nil)
 '(global-auto-complete-mode t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(jira-url "https://jira.talksum.com/rpc/xmlrpc")
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))
 '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
 '(org-startup-folded nil)
 '(python-honour-comment-indentation t)
 '(rdebug-restore-original-window-configuration t)
 '(ruby-indent-level 4)
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(tags-table-list (quote ("/Users/gfoster/projects/talksum/CAMEL/TAGS")))
 '(tool-bar-mode nil)
 '(tramp-default-method "scpx")
 '(tramp-encoding-shell "/bin/bash")
 '(tramp-verbose 4)
 '(vc-handled-backends (quote (Git RCS SVN Bzr Hg Arch)))
 '(yaml-indent-offset 4)
 '(yas/snippet-dirs (quote ("/Users/gfoster/emacs.d/yasnippet/snippets")) nil (yasnippet)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "grey75" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "apple-menlo"))))
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


