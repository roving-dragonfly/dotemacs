;; Package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
;; Use-package to load all the others packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; GUI cleanup
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Store the pollution
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Random
(fset 'yes-or-no-p 'y-or-n-p)
(load-theme 'metalheart)

;; Better defaults
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Diminish
(use-package diminish
  :ensure t)

;; Helm stuff
(use-package helm
  :ensure t
  :init (progn
	  (use-package helm-projectile :ensure t))
  :bind (("M-x"       . helm-M-x)
	 ("C-x C-f"   . helm-find-files)
	 ("M-y"       . helm-show-kill-ring)
	 ("C-x b"     . helm-mini)
	 ("C-h a"     . helm-apropos)
	 ("C-c h"     . helm-command-prefix))
  :config (progn
	    (require 'helm-config)
	    (global-unset-key (kbd "C-x c"))
	    (helm-mode 1)
	    (setq helm-split-window-in-side-p t
		  helm-autoresize-max-height 42
		  helm-autoresize-min-height 0
		  helm-move-to-line-cycle-in-source t
		  helm-ff-search-library-in-sexp t
		  helm-M-x-fuzzy-match t
		  Helm-buffers-fuzzy-matching t
		  helm-lisp-fuzzy-completion t
		  helm-apropos-fuzzy-match t
		  helm-recentf-fuzzy-match t)))


;; Projectile
(use-package projectile
  :ensure t
  :diminish
  :init (projectile-global-mode)
  :bind (("<f12>" . projectile-compile-project))
  :config (progn
	    (setq projectile-completion-system 'helm)))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;; C
(use-package helm-gtags
  :ensure t
  :diminish
  :init (progn
	  (setq
	   helm-gtags-ignore-case t
	   helm-gtags-auto-update t
	   helm-gtags-use-input-at-cursor t
	   helm-gtags-pulse-at-cursor t
	   helm-gtags-prefix-key "\C-cg"
	   helm-gtags-suggested-key-mapping t)
	  (unbind-key "C-M-j" c-mode-map))
  :bind (("C-c g a"  . helm-gtags-tags-in-this-function)
	 ("C-M-j"    . helm-gtags-select)
	 ("M-."      . helm-gtags-dwim)
	 ("M-,"      . helm-gtags-pop-stack))
  :config (progn
	    (add-hook 'dired-mode-hook 'helm-gtags-mode)
	    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
	    (add-hook 'c-mode-hook 'helm-gtags-mode)
	    (add-hook 'c++-mode-hook 'helm-gtags-mode)
	    (add-hook 'asm-mode-hook 'helm-gtags-mode)))

(use-package function-args
  :ensure t
  :config (fa-config-default)
  )

;; Undo tree
(use-package undo-tree
  :ensure t
  :diminish
  :config (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; Guide key
(use-package guide-key
  :ensure t
  :config (progn
	    (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h")
		  guide-key/idle-delay 0.4
		  guide-key/recursive-key-sequence-flag t
		  guide-key/popup-window-position 'right)
	    (guide-key-mode 1)))
