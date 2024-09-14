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
;;                '(internal-border-width . 10)
;;                '(left-fringe    . 1)
;;                '(right-fringe   . 1)
;;                '(tool-bar-lines . 0)
;;                '(menu-bar-lines . 0))))

;; Set Transparency (pre Emacs-29)
;; (set-frame-parameter (selected-frame) 'alpha '(90 90))
;; (add-to-list 'default-frame-alist '(alpha 90 90))

(menu-bar-mode -1)
(setq ring-bell-function 'ignore)

(column-number-mode)
(global-display-line-numbers-mode t)
(global-visual-line-mode 1)

(setq auto-save-default nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 5)

;; Don't use line numbers in these modes!
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
        eat-mode-hook
        eww-mode-hook
        help-mode-hook
        elfeed-search-mode-hook
        elfeed-show-mode-hook
        devdocs-mode-hook
        treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (setq-default cursor-type 'bar)

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
(set-face-attribute 'default nil :height 130)

(global-set-key (kbd "C-x C-b") 'ibuffer)
                                        ;
(electric-pair-mode 1)

;; Control warning level (C-h v "warning-minimum-level")
;; This is to stop the pesky warning messages from popping a buffer up.
;; However, maybe they should be more of a concern...
(setq warning-minimum-level :error)

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
    (split-window-below -15)
    (other-window 1)
    (eshell))

(defun stm/flymake-mode-hook ()
  "Personal flymake-mode hook"
  (define-key flymake-mode-map (kbd "C-h ,") 'flymake-show-buffer-diagnostics))

(add-hook 'flymake-mode-hook 'stm/flymake-mode-hook)

(global-set-key (kbd "C-x 2") #'stm/split-horizontally)
(global-set-key (kbd "C-x 3") #'stm/split-vertically)
(global-set-key (kbd "C-c e") #'stm/split-eshell)

;;(set-face-attribute 'default nil :family "Ubuntu Mono")
(set-face-attribute 'default nil :height 100)
;;(set-face-attribute 'variable-pitch nil :family "Iosevka")

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


(use-package ef-themes
  :config
  (load-theme 'ef-owl))

(use-package modus-themes
  :config
  (setq modus-themes-common-palette-overrides
        '((fringe unspecified)
          (fg-line-number-active fg-main)
          (fg-line-number-inactive "gray50")
          (bg-line-number-active unspecified)
          (bg-line-number-inactive unspecified)

          (fg-region unspecified)

          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))

  (setq modus-operandi-palette-overrides
        `(,@modus-themes-common-palette-overrides
          ,@modus-themes-preset-overrides-intense))

  (setq modus-vivendi-palette-overrides
        `(,@modus-themes-common-palette-overrides
          ,@modus-themes-preset-overrides-faint))
  
  (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
  (setq modus-themes-italic-constructs t))

;; (use-package catppuccin-theme
;;   :config
;;   (setq catppuccin-italic-comments t)
;;   (setq catppuccin-highlight-matches t)
;;   (setq catppuccin-flavor 'latte)
;;   ;; Change comment color to something more readable
;;   (catppuccin-set-color 'surface2 "#7b97d1" 'mocha)
;;   (catppuccin-set-color 'surface2 "#8897b3" 'latte)
;;   ;;(catppuccin-reload)
;;   )

(use-package adwaita-dark-theme
  :config
  (eval-after-load 'flymake #'adwaita-dark-theme-flymake-fringe-bmp-enable)
  (eval-after-load 'diff-hl #'adwaita-dark-theme-diff-hl-fringe-bmp-enable)
  (adwaita-dark-theme-arrow-fringe-bmp-enable)
  (setq adwaita-dark-theme-bold-vertico-current t)
  )

(use-package standard-themes
  :config
  (setq standard-themes-italic-constructs t))

(defun stm/toggle-theme ()
  "Toggles between a chosen light and dark theme"
  (interactive)
  (if (eq (car custom-enabled-themes) 'ef-owl)
      (progn (disable-theme 'ef-owl)
             (load-theme 'ef-eagle))
    (progn (disable-theme 'ef-eagle)
           (load-theme 'ef-owl))))

(defun catppuccin-toggle-theme ()
  "Toggle between catppuccin-themes"
  (interactive)
  (if (eq catppuccin-flavor 'mocha)
      (setq catppuccin-flavor 'latte)
    (setq catppuccin-flavor 'mocha))
  (catppuccin-reload))

;; Mood-line
(use-package mood-line
  ;:config
  ;(mood-line-mode)
  )

(use-package minions
  :config
  (minions-mode 1))

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
  ("M-g i" . consult-imenu)
  ;; M-s bindings (search-map)
  ("M-s d" . consult-find)
  ("M-s D" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines) 
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; Embark
(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim))

(use-package embark-consult
  :after embark)

;; Dashboard
(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/.emacs.d/emacs-blackhole.png")
  (setq dashboard-banner-logo-title "~=Enter the Void=~")
  (dashboard-setup-startup-hook))

(setf dashboard-projects-backend 'project-el
      dashboard-items '((projects . 5) (recents . 5) (agenda . 5)
			            (bookmarks . 5)))

;; Yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet)

;; Company
(use-package company
  :bind (:map prog-mode-map
              ("C-i" . company-indent-or-complete-common)
              ("C-M-i" . company-complete))
  :init
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 8)
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
  (setq which-key-idle-delay 2.0))

(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :config
  (add-hook 'marginalia-mode-hook
            #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode))


;; Projectile
;; (use-package projectile
;;  :diminish projectile-mode
;;  :config (projectile-mode)
;;  :custom ((projectile-completion-system 'default))
;;  :bind (:map projectile-mode-map
;;  ("C-c p" . projectile-command-map))
;;  :init
;;  (when (file-directory-p "~/repos")
;;    (setq projectile-project-search-path '("~/repos")))
;;  (setq projectile-switch-project-action #'projectile-dired))

;; Magit
(use-package magit
  :if
  (executable-find "git")
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :config
  (meow-setup)
  (setq meow-keypad-describe-keymap-function nil)
  (setq meow-keypad-leader-dispatch "C-c")
  (meow-global-mode 1))

;; Git Gutter
;; Credit to Ian Y.E Pan for these code snippets
;; (use-package git-gutter
;;   :hook
;;   (prog-mode . git-gutter-mode)
;;   :config
;;   (setq git-gutter:update-interval 0.02)
;;   (setq git-gutter:window-width 1)
;;   ;; Note that this is only to make git gutter work for Non-Doom themes
;;   ;; Comment this out and uncomment git-gutter-fringe for Doom-Themes
;;   ;; (setq git-gutter:added-sign " "
;;   ;;       git-gutter:deleted-sign " "
;;   ;;       git-gutter:modified-sign " ")
;;   )

;; (use-package git-gutter-fringe
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil
;;     '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil
;;     '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil
;;     'bottom))

;; Treemacs
(use-package treemacs
  :bind ("C-c s" . treemacs)
  :config
  (setq treemacs-width 30)
  (setq treemacs-position 'right)
  (treemacs-project-follow-mode 1))

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

;; Ace-Window
(global-set-key (kbd "C-c o") 'ace-window)

;; Move text up and down
(use-package move-text
  :bind
  ("M-p" . 'move-text-up)
  ("M-n" . 'move-text-down))

;; Emulate-a-Terminal
;; M-x eat-compile-terminfo on MacOS to fix 'delete' issue
(use-package eat
  :bind (("M-RET" . eat))
  :hook
  (eshell-mode . eat-eshell-mode)
  :config
  ;; Close the terminal buffer when the shell terminates.
  (setq eat-kill-buffer-on-exit t)
  ;; Enable mouse-support.
  (setq eat-enable-mouse t)
  ;; Disable Exit Code
  (setq eat-enable-shell-prompt-annotation nil))

;; Tab-Bar (Window workspaces)
(setq tab-bar-show nil)
(tab-bar-mode 1)

;; Matrix + IRC
;; (use-package ement)

;; RSS
;; (use-package elfeed
;;   :config
;;   (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
;;         elfeed-show-entry-switch 'display-buffer)
;;   (setq elfeed-feeds
;;         '(("https://thephd.dev/feed.xml" tech c++)
;;           ("https://takeonrules.com/feed.xml" tech)
;;           ("https://www.rousette.org.uk/index.xml" tech emacs)
;;           ("https://lepisma.xyz/atom.xml" tech emacs)
;;           ("https://karthinks.com/index.xml" tech emacs)
;;           ("https://unixsheikh.com/feed.rss" tech unix foss)
;;           ("https://sizeof.cat/index.xml" tech security)
;;           ("https://inconvergent.net/atom.xml" tech art lisp)
;;           ("https://chollinger.com/blog/index.xml" tech)
;;           ("https://nullprogram.com/feed/" tech c)
;;           ("https://two-wrongs.com/feed" tech math)
;;           ("https://geo-ant.github.io/blog/feed.xml" tech c++ rust)
;;           ("https://fasterthanli.me/index.xml" tech c++ rust)
;;           ("https://thenumb.at/feed.xml" tech graphics)
;;           ("https://batsov.com/atom.xml" tech ruby lisp ocaml)
;;           ("https://matt.might.net/articles/feed.rss" tech plang)))
;;   :bind
;;   ("C-x w" . elfeed))

;; (use-package popper
;;   :bind (("C-;"   . popper-toggle-latest)
;;          ("M-;"   . popper-cycle)
;;          ("C-M-;" . popper-toggle-type))
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$"
;;           "\\*Async Shell Command\\*"
;;           help-mode
;;           compilation-mode
;;           eshell-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-c D" . docker))

;; Verb (HTTP Client + Org)
;; (use-package verb)

;; RESTClient (More simple HTTP Client)
(use-package restclient)

;; RMSBolt (Godbolt)
(use-package rmsbolt)

;; Dumb-Jump
(use-package dumb-jump
  :init
  (setq dumb-jump-prefer-searcher 'rg)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-disable-obsolete-warnings t)
  (dumb-jump-mode))

;; Undo
(use-package vundo
  :bind
  ("C-c v" . vundo))

;; Direnv Integration
(use-package envrc
  :if (executable-find "direnv")
  :hook (after-init . envrc-global-mode))

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
  ;;(define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  ;;(stm/org-font-setup)
  )

;; (use-package org-modern
;;   :after org
;;   :hook
;;   (org-mode . org-modern-mode)
;;   :config
;; ;;  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
;;   (setq
;;    ;; Edit settings
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-pretty-entities-include-sub-superscripts nil))

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
;; (use-package paredit
;;   :hook
;;   (lisp-mode . paredit-mode)
;;   (scheme-mode . paredit-mode))

;; Documentation
(use-package devdocs
  :bind
  ("C-h D" . devdocs-lookup))

(defun stm/disable-visual-line-mode ()
  "Disable `visual-line-mode' in certain buffers."
  (when (or (derived-mode-p 'elfeed-search-mode)
            (derived-mode-p 'elfeed-show-mode)
            (derived-mode-p 'devdocs-mode))
    (visual-line-mode -1)))

(add-hook 'after-change-major-mode-hook 'stm/disable-visual-line-mode)

;; Sideline
(use-package sideline-flymake)
(use-package sideline
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line)
  (setq sideline-backends-right '(sideline-flymake)
        sideline-priority 100))

;; LSP + Languages

(defun stm/toggle-eglot ()
  "Toggle Eglot on and off."
  (interactive)
  (if (eglot-managed-p)
      (call-interactively 'eglot-shutdown)
    (call-interactively 'eglot)))

;; (use-package apheleia
;;   :hook
;;   ((apheleia-mode . python-mode)
;;    (apheleia-mode . c++-mode)
;;    (apheleia-mode . c++ts-mode))
;;   :config
;;   (setq apheleia-formatters
;;         '((python-mode . black)
;;           (c++-mode . clang-format)
;;           (c++ts-mode . clang-format)))
;;)

;; Eglot
(use-package eglot
  :bind
  ("C-c l" . 'stm/toggle-eglot)
  ("C-c a" . 'eglot-code-actions)
  :config
  ;;(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; gem install solargraph
  (add-to-list 'eglot-server-programs
               '((ruby-mode) "solargraph"))
  (add-to-list 'eglot-server-programs '((web-mode . ("typescript-language-server" "--stdio"))))
  (setq eglot-connect-timeout 60)

  (setq eglot-ignored-server-capabilities '(:hoverProvider))
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(c++-mode . ("clangd" "--header-insertion=never"))))
;; (add-hook 'eglot-managed-mode-hook 
;;        (lambda () (setq eldoc-documentation-strategy 
;;                         #'eldoc-documentation-compose)))

(use-package eldoc-box
  :bind
  ("C-h /" . 'eldoc-box-help-at-point)
  :config
  (setq eldoc-box-max-pixel-height 500
        eldoc-box-max-pixel-width  500))

;;(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)

(defun my-eldoc-box-setup ()
  (set-face-attribute 'eldoc-box-body nil :font (face-attribute 'default :font)))

(add-hook 'eldoc-box-hover-mode-hook 'my-eldoc-box-setup)


;; Python
(use-package python-mode)

;; Ruby
;;(add-hook 'ruby-mode-hook 'eglot-ensure)
(use-package robe
  :hook
  (ruby-mode . robe-mode))

;; Common Lisp
(use-package sly)

;; Scheme - Guile
;; (use-package geiser)

;; (use-package geiser-guile
;;   :after geiser)

;; Clojure
(use-package clojure-mode)
(use-package cider
  :after clojure-mode
  :hook
  (clojure-mode . cider-mode))

;; Rust
(use-package rust-mode)

;; C++ (cpp)
(add-to-list 'eglot-server-programs
	     '((c++-mode) "clangd"))
;;(add-hook 'c++-mode-hook 'eglot-ensure)

(use-package cmake-mode)
(use-package meson-mode)

;; C
(add-to-list 'eglot-server-programs
             '((c-mode) "ccls"))
;;(add-hook 'c-mode-hook 'eglot-ensure)

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

(use-package typescript-mode)

;; Yaml
(use-package yaml-mode)

;; Scala
;; (use-package scala-mode
;;   :interpreter
;;   ("scala" . scala-mode))

;; Coq
(use-package proof-general)
(use-package company-coq
  :hook
  (coq-mode . company-coq))

;; Haskell
(use-package haskell-mode)
 ;; :hook
 ;; (haskell-mode . haskell-unicode-input-method-enable))

;; OCaml
(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))
(use-package dune)
(use-package merlin
  :hook
  (tuareg-mode. merlin-mode)
  (merlin-mode . company-mode)
  :config
  (setq merlin-eldoc-occurrences nil))

(use-package merlin-eldoc
  :hook
  ((tuareg-mode) . merlin-eldoc-setup))

(use-package utop
  :hook
  (tuareg-mode . utop-minor-mode))

(use-package reason-mode)

;; Crystal
;; (use-package crystal-mode
;;   :interpreter
;;   ("crystal" . crystal-mode)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.cr$" . crystal-mode)))

;; Julia
;; (use-package julia-mode)

;; (use-package julia-repl
;;   :after julia-mode
;;   :bind
;;   (:map julia-mode-map
;;         ("C-c C-e" . julia-repl-send-line)
;;         ("C-c C-b" . julia-repl-send-buffer)
;;         ("C-c C-r" . julia-repl-send-region))
;;  )

;; Elixir
(use-package elixir-mode)

;; Zig
(use-package zig-mode)

;; Golang
(use-package go-mode)

;; Lua
(use-package lua-mode)

;; Nix
(use-package nix-mode)

;; Protobuf
(use-package protobuf-mode)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
