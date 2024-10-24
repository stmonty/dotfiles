(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1)   ;; Disable the toolbar
(tooltip-mode -1)    ;; Disable the tooltips
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)

(column-number-mode)
(global-display-line-numbers-mode t)
(global-visual-line-mode 1)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(electric-pair-mode 1)

(load-theme 'modus-vivendi)

;; Control warning level (C-h v "warning-minimum-level")
;; This is to stop the pesky warning messages from popping a buffer up.
;; However, maybe they should be more of a concern...
(setq warning-minimum-level :error)


(fido-vertical-mode 1)
