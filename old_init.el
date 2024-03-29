;; Monty's Personal Emacs Config

;; Garbage-Collection
(setq gc-cons-threshold (* 20 1024 1024))
;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1)   ;; Disable the toolbar
(tooltip-mode -1)    ;; Disable the tooltips
(set-fringe-mode 10)

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

;; IBuffer Keybind
(global-set-key (kbd "C-c i") 'ibuffer)


;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq ruby-indent-level 4)
(setq-default c-basic-offset 4)

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

;; MacOS specific
(when (eq system-type 'darwin)
  (load-file "~/.emacs.d/macos.el"))

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Doom Themes
(use-package doom-themes
  :init (load-theme 'doom-palenight))

;; Doom-Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Moody
;; (use-package moody
;;     :config
;;     (moody-replace-mode-line-buffer-identification)
;;     (moody-replace-vc-mode))

;; (use-package minions
;;   :config
;;   (minions-mode 1))

;; All The Icons
;; Make sure to run 'M-x all-the-icons-install-fonts'
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Ivy
(use-package ivy

  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-n" . ivy-next-line)
	 ("C-p" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))


(use-package god-mode
  :bind
  ("C-c z" . god-mode)
  :config
  (define-key god-local-mode-map (kbd "i") #'god-local-mode))

;; Hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
	  "Scale Text"
	  ("j" text-scale-increase "in")
	  ("k" text-scale-decrease "out")
	  ("f" nil "finished" :exit t))


;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))


;; IBuffer
;;(use-package ibuffer-projectile
;;  :after projectile)

(use-package ibuffer-vc)

;; Magit
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(setf dashboard-projects-backend 'projectile
      dashboard-items '((projects . 5) (recents . 5) (agenda . 5)
			(bookmarks . 5)))

;; Company
(use-package company
  :bind (:map prog-mode-map
              ("C-i" . company-indent-or-complete-common)
              ("C-M-i" . counsel-company))
  :hook
  (prog-mode . company-mode) ;;Start in all programming buffers
  :config
  (setq company-idle-delay 0.2)
  (setq company-tooltip-limit 5)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-require-match 'never))

;; Git Gutter
;; Credit to Ian Y.E Pan for these code snippets
(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

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
  :bind ("C-c S" . treemacs))

;; Dired
(use-package dired-sidebar
  :bind ("C-c s" . dired-sidebar-toggle-sidebar))


;; Dirvish
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  ("C-c d" . dirvish))

;; EShell


;; Docker
(use-package docker
  :ensure t
  :bind ("C-c D" . docker))

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
(use-package cider)

;; Rust
(use-package rust-mode)


;; C/C++
(add-to-list 'eglot-server-programs
	     '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Javascript/Typescript

;; Scala
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))


;; Java

;; Elixir
;;(use-package elixir-mode)

;; Crystal
;;(use-package crystal-mode)
