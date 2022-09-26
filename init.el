;; Monty's Personal Emacs Config

;; Garbage-Collection
(setq gc-cons-threshold 1000000)
;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(setq read-process-output-max (* 1024 1024))

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1)   ;; Disable the toolbar
(tooltip-mode -1)    ;; Disable the tooltips
(setq default-frame-alist
      (append (list
	           '(min-height . 1)
               '(height     . 45)
	           '(min-width  . 1)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 1)
               '(right-fringe   . 1)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))


(menu-bar-mode -1)
(setq ring-bell-function 'ignore)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq auto-save-default nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 5)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Custom variables
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set backup by copying to preserve hard-links
(setq backup-by-copying-when-linked t)

;; Pair Braces and Brackets
(setq electric-pair-pairs '(
			    (?\{ . ?\})
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\" . ?\")
			    ))

;; Set font size
(set-face-attribute 'default nil :height 120)


(electric-pair-mode 1)


;; Make escape quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-relative)
;; Controls tab-width in Ruby-mode
(setq ruby-indent-level 2)
(setq-default c-basic-offset 4)

;; Splitting of Windows
(defun stm/split-horizontally ()
    "Split window below"
  (interactive)
  (split-window-below)
  (other-window 1))

(defun stm/split-vertically ()
    "Split window right"
  (interactive)
  (split-window-right)
  (other-window 1))

(defun stm/split-eshell ()
    "Opens eshell in a bottom window"
    (interactive)
    (stm/split-horizontally)
    (eshell))

(global-set-key (kbd "C-x 2") #'stm/split-horizontally)
(global-set-key (kbd "C-x 3") #'stm/split-vertically)
(global-set-key (kbd "C-c e") #'stm/split-eshell)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;; MacOS specific
(when (eq system-type 'darwin)
  (load-file "~/.emacs.d/macos.el"))

;; exec-path-from-shell
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Doom Themes
(use-package doom-themes
  :init
  (setq doom-vibrant-brighter-modeline t)
  (setq doom-city-lights-brighter-modeline t
        doom-city-lights-brighter-comments t
        doom-city-lights-comment-bg nil)
;;  (setq doom-dracula-brighter-modeline t)
  (load-theme 'doom-dracula)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Doom-Modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

;; Moody Modeline
(use-package moody
    :config
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode))

;; Minions - To control minor modes in modeline
(use-package minions
  :config
  (minions-mode 1))

;; All The Icons
;; Make sure to run 'M-x all-the-icons-install-fonts'
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :config
  (rainbow-mode +1))

(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; Completions
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode 1))

(use-package consult
  :bind
  ;; C-# bindings
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ;; M-# bindings
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ;; M-s bindings (search-map)
  ("M-s d" . consult-find)
  ("M-s D" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines) 
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; Company
(use-package company
  :bind (:map prog-mode-map
              ("C-i" . company-indent-or-complete-common)
              ("C-M-i" . company-complete))
  :init
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 5)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-require-match 'never)
  (global-company-mode))

(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :config
  (add-hook 'marginalia-mode-hook
            #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode))


;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'default))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Magit
(use-package magit
  :if
  (executable-find "git")
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Dashboard
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook))

;; (setf dashboard-projects-backend 'projectile
;;       dashboard-items '((projects . 5) (recents . 5) (agenda . 5)
;; 			(bookmarks . 5)))


;; Git Gutter
;; Credit to Ian Y.E Pan for these code snippets
(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))
  ;; Note that this is only to make git gutter work for Non-Doom themes
  ;; Comment this out and uncomment git-gutter-fringe for Doom-Themes
  ;;(setq git-gutter:added-sign " "))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil
    '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil
    '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil
    'bottom))

;; Treemacs
(use-package treemacs
  :bind ("C-c s" . treemacs))

;; Dired
(use-package dired-sidebar
  :bind ("C-c S" . dired-sidebar-toggle-sidebar))

;; Centaur Tabs
;; (use-package centaur-tabs
;;   :init
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-gray-out-icons 'buffer)
;;   (setq centaur-tabs-set-bar 'left)
;;   (setq centaur-tabs-style "bar")
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))


;; Dirvish
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  ("C-c d" . dirvish))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-c D" . docker))

;; Dumb-Jump
(use-package dumb-jump
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :init
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-disable-obsolete-warnings t)
  (dumb-jump-mode))


;; Org Mode and Roam
(defun stm/org-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1))

(defun stm/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  ;; (dolist (face '((org-level-1 . 1.2)
  ;;                 (org-level-2 . 1.1)
  ;;                 (org-level-3 . 1.05)
  ;;                 (org-level-4 . 1.0)
  ;;                 (org-level-5 . 1.1)
  ;;                 (org-level-6 . 1.1)
  ;;                 (org-level-7 . 1.1)
  ;;                 (org-level-8 . 1.1)))
  ;;   (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook
  (org-mode . stm/org-setup)
  :config
  (setq org-ellipsis " ▾")
  (stm/org-font-setup)
  )

(use-package org-roam
  :custom
  (org-roam-directory "~/roam")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; LSP + Languages

;; Eglot
(use-package eglot)

;; Ruby
;; gem install solargraph
(add-hook 'ruby-mode-hook 'eglot-ensure)
;; (use-package robe
;;   :hook
;;   (ruby-mode . robe-mode))

;; Common Lisp
(use-package sly)

;; Scheme - Guile
(use-package geiser)

(use-package geiser-guile
  :after geiser)

;; Clojure
(use-package clojure-mode)
(use-package cider
  :after clojure-mode)

;; Rust
(use-package rust-mode)

;; C++
(add-to-list 'eglot-server-programs
	     '((c++-mode) "clangd"))
(add-hook 'c++-mode-hook 'eglot-ensure)

;; C
(add-to-list 'eglot-server-programs
             '((c-mode) "ccls"))
(add-hook 'c-mode-hook 'eglot-ensure)

;; Javascript/Typescript
;; (use-package tide
;;   :hook
;;   ((typescript-mode . tide-setup)
;;    (typescript-mode . tide-hl-identifier-mode)
;;    (before-save . tide-format-before-save)))

;; Scala
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; Haskell
(use-package haskell-mode
  :hook
  (haskell-mode . eglot-ensure))

;; Java

;; Elixir
;;(use-package elixir-mode)

;; Crystal
;;(use-package crystal-mode)
