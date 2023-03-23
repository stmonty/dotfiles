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
;; (setq default-frame-alist
;;       (append (list
;; 	           '(min-height . 1)
;;                '(height     . 45)
;; 	           '(min-width  . 1)
;;                '(width      . 81)
;;                '(vertical-scroll-bars . nil)
;;                '(internal-border-width . 24)
;;                '(left-fringe    . 1)
;;                '(right-fringe   . 1)
;;                '(tool-bar-lines . 0)
;;                '(menu-bar-lines . 0))))

;; Set Transparency
;;(set-frame-parameter (selected-frame) 'alpha '(95 95))
;;(add-to-list 'default-frame-alist '(alpha 95 95))

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
		eshell-mode-hook
        eat-mode-hook
        devdocs-mode-hook
        treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default cursor-type 'bar)


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
(add-hook 'find-file-hook (lambda () (set-face-attribute 'default nil :height 130)))

;; Compilation
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c x") 'shell-command)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(electric-pair-mode 1)

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

;;(set-face-attribute 'default nil :family "Iosevka")
;;(set-face-attribute 'variable-pitch nil :family "Iosevka")
(set-face-attribute 'default nil :height 110)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
             ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

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
  (setq doom-gruvbox-dark-variant "hard")
  (setq doom-themes-enable-italic t)
  (setq doom-themes-enable-bold t)
  :config
  (setq doom-vibrant-brighter-modeline t)
  (setq doom-vibrant-brighter-comments t)
  (setq doom-one-brighter-comments t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config)
  (load-theme 'doom-vibrant))

;; Ef-Themes
;; (use-package ef-themes)

(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
        '((fringe unspecified)
          (fg-line-number-active fg-main)
          (fg-line-number-inactive "gray50")
          (bg-line-number-active unspecified)
          (bg-line-number-inactive unspecified)

          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))

  (setq modus-vivendi-palette-overrides
        `(,@modus-themes-common-palette-overrides
          ,@modus-themes-preset-overrides-faint))
  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi)))


(defun stm/toggle-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-vibrant)
      (progn (disable-theme 'doom-vibrant)
             (load-theme 'doom-one-light))
    (progn (disable-theme 'doom-one-light)
           (load-theme 'doom-vibrant))))

(global-set-key (kbd "C-c t") #'modus-themes-toggle)
(global-set-key (kbd "C-c T") #'stm/toggle-theme)

;; Mood-line
(use-package mood-line
  :init
  (mood-line-mode))

;; All The Icons
;; Make sure to run 'M-x all-the-icons-install-fonts'
(use-package all-the-icons
  :if window-system
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :config
  (rainbow-mode +1))

;; Minimap
;; (use-package minimap
;;   :bind
;;   ("C-c m" . minimap-mode)
;;   :config
;;   (setq minimap-window-location 'right))

;; (use-package highlight-indent-guides
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq highlight-indent-guides-method 'character))

;; Completions
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; (use-package vertico-buffer
;;   :after vertico
;;   :custom
;;   (vertico-buffer-hide-prompt t)
;;   :config
;;   ;; put minibuffer at top -- this is the more natural place to be looking!
;;   (setq vertico-buffer-display-action
;;         '(display-buffer-in-side-window
;;           (window-height . 13)
;;           (side . top)))
;;   (vertico-buffer-mode 1))

;; (when (display-graphic-p)
;;   (use-package vertico-posframe
;;     :after vertico
;;     :init
;;     (vertico-posframe-mode 1)
;;     :config
;;     (setq vertico-posframe-parameters
;;           '((left-fringe . 8)
;;             (right-fringe . 8)))
;;     ))

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

;; Dashboard
(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/.emacs.d/emacs-blackhole.png")
  (setq dashboard-banner-logo-title "~=Enter the Void=~")
  (dashboard-setup-startup-hook))

(setf dashboard-projects-backend 'projectile
      dashboard-items '((projects . 5) (recents . 5) (agenda . 5)
			(bookmarks . 5)))

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

(defun stm/company-backend-with-yas (backends)
      (if (and (listp backends) (memq 'company-yasnippet backends))
	  backends
	(append (if (consp backends)
		    backends
		  (list backends))
		'(:with company-yasnippet))))

;; add yasnippet to all backends
(setq company-backends
      (mapcar #'stm/company-backend-with-yas company-backends))

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


;; Git Gutter
;; Credit to Ian Y.E Pan for these code snippets
(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02)
  (setq git-gutter:window-width 1)
  ;; Note that this is only to make git gutter work for Non-Doom themes
  ;; Comment this out and uncomment git-gutter-fringe for Doom-Themes
  (setq git-gutter:added-sign " "
        git-gutter:deleted-sign " "
        git-gutter:modified-sign " "))

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

;; Tab-Bar (Window workspaces)
(setq tab-bar-show nil)
(tab-bar-mode 1)

(use-package popper
  :bind (("C-;"   . popper-toggle-latest)
         ("M-;"   . popper-cycle)
         ("C-M-;" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          eshell-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-c D" . docker))

;; Dumb-Jump
(use-package dumb-jump
  :init
  (setq dumb-jump-force-searcher 'rg)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-disable-obsolete-warnings t)
  (dumb-jump-mode))

;; Undo
(use-package vundo)

;; Emacs-Eat
(use-package eat
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode)
  :bind
  ("C-c v" . eat))

(use-package markdown-mode)

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
  (setq org-return-follows-link t)
  (setq org-startup-truncated nil)
;;  (stm/org-font-setup)
  )

(use-package org-modern
  :after org
  :hook
  (org-mode . org-modern-mode)
  :config
;;  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts nil))

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

;; Paredit
(use-package paredit
  :hook
  (lisp-mode . paredit-mode)
  (scheme-mode . paredit-mode))

;; Codeium (AI Completion)
;; we recommend using use-package to organize your init.el
;; (use-package codeium
;;     ;; if you use straight
;;     ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
;;     ;; otherwise, make sure that the codeium.el file is on load-path
;;     :load-path "~/.emacs.d/codeium.el"
;;     :init
;;     ;; use globally
;;     (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;     ;; or on a hook
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;     ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions
;;     ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;     ;; an async company-backend is coming soon!

;;     ;; codeium-completion-at-point is autoloaded, but you can
;;     ;; optionally set a timer, which might speed up things as the
;;     ;; codeium local language server takes ~0.2s to start up
;;     ;; (add-hook 'emacs-startup-hook
;;     ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;     ;; :defer t ;; lazy loading, if you want
;;     :config
;;     (setq use-dialog-box nil) ;; do not use popup boxes

;;     ;; if you don't want to use customize to save the api-key
;;     ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;     ;; get codeium status in the modeline
;;     (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;     (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;     ;; alternatively for a more extensive mode-line
;;     ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;     ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;     (setq codeium-api-enabled
;;         (lambda (api)
;;             (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;     ;; you can also set a config for a single buffer like this:
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local codeium/editor_options/tab_size 4)))

;;     ;; You can overwrite all the codeium configs!
;;     ;; for example, we recommend limiting the string sent to codeium for better performance
;;     (defun my-codeium/document/text ()
;;         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;     ;; if you change the text, you should also change the cursor_offset
;;     ;; warning: this is measured by UTF-8 encoded bytes
;;     (defun my-codeium/document/cursor_offset ()
;;         (codeium-utf8-byte-length
;;             (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;     (setq codeium/document/text 'my-codeium/document/text)
;;     (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))


;; Documentation
(use-package devdocs
  :hook
  (devdocs-mode . visual-line-mode)
  :bind
  ("C-h D" . devdocs-lookup))


;; Yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet)

;; LSP + Languages

;; Eglot
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; gem install solargraph
  (add-to-list 'eglot-server-programs
               '((ruby-mode) "solargraph"))
  :custom
  (setq eglot-connect-timeout 60))

;; Python
(use-package python-mode
  :hook
  (python-mode . eglot-ensure))
(use-package apheleia
  :hook
  (apheleia-mode . python-mode))

;; Ruby
;;(add-hook 'ruby-mode-hook 'eglot-ensure)
(use-package robe
  :hook
  (ruby-mode . robe-mode))

;; Common Lisp
(use-package sly)

;; Scheme - Guile
(use-package geiser)

(use-package geiser-guile
  :after geiser)

;; Clojure
(use-package clojure-mode)
(use-package cider
  :after clojure-mode
  :hook
  (clojure-mode . cider-mode))

;; Rust
(use-package rust-mode
  :hook
  (rust-mode . eglot-ensure))

;; C++
(add-to-list 'eglot-server-programs
	     '((c++-mode) "clangd"))
(add-hook 'c++-mode-hook 'eglot-ensure)

;; C
(add-to-list 'eglot-server-programs
             '((c-mode) "ccls"))
(add-hook 'c-mode-hook 'eglot-ensure)

;; Web Development
;; (use-package tide
;;   :hook
;;   ((typescript-mode . tide-setup)
;;    (typescript-mode . tide-hl-identifier-mode)
;;    (before-save . tide-format-before-save)))
(use-package web-mode
  :config
  (progn
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.html"   . web-mode))
      (add-to-list 'auto-mode-alist '("\\.css"    . web-mode))
      ;; this magic incantation fixes highlighting of jsx syntax in .js files
      (setq web-mode-content-types-alist
            '(("jsx" . "\\.js[x]?\\'"))))
  )

(use-package js2-mode)

(use-package typescript-mode
  :hook
  (typescript-mode . eglot-ensure))
(add-to-list 'eglot-server-programs '((web-mode . ("typescript-language-server" "--stdio"))))

;; Yaml
(use-package yaml-mode)

;; Scala
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; Haskell
(use-package haskell-mode
  :hook
  (haskell-mode . eglot-ensure))

;; Crystal
(use-package crystal-mode
  :interpreter
  ("crystal" . crystal-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.cr$" . crystal-mode)))

;; Julia
(use-package julia-mode)

(use-package julia-repl
  :after julia-mode
  :bind
  (:map julia-mode-map
        ("C-c C-e" . julia-repl-send-line)
        ("C-c C-b" . julia-repl-send-buffer)
        ("C-c C-r" . julia-repl-send-region))
  )

;; Golang
(use-package go-mode)

;; Zig
(use-package zig-mode)

;; Lua
(use-package lua-mode)

