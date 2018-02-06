;;; Config emacs

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
(load-theme 'metalheart t)

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
	 ("C-S"       . helm-occur)
	 ("C-c h"     . helm-command-prefix))
  :config (progn
	    (require 'helm-config)
	    (global-unset-key (kbd "C-x c"))
	    (helm-mode 1)
	    (setq helm-split-window-inside-p t
		  helm-autoresize-max-height 42
		  helm-autoresize-min-height 0
		  helm-move-to-line-cycle-in-source t
		  helm-ff-search-library-in-sexp t)))

;; Projectile
(use-package projectile
  :ensure t
  :diminish
  :init (projectile-mode)
  :bind (("<f12>" . projectile-compile-project))
  :config (progn
	    (setq projectile-completion-system 'helm)))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Company
(use-package company
  :ensure t
  :init (global-company-mode)
  :config (progn
	    (setq company-tooltip-limit 20)
	    (setq company-idle-delay 0.2)))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
	   helm-gtags-suggested-key-mapping t))
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
  :config (progn
	    (global-semantic-idle-summary-mode 1)
	    (fa-config-default)))
;; Clojure

(use-package clojure-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :config (progn
			(add-hook 'clojure-mode-hook 'cider-mode)
			(add-hook 'cider-repl-mode-hook #'company-mode)
			(add-hook 'cider-mode-hook #'company-mode)))

;; Orgmode
(use-package org
  :bind (("C-c c"  . org-capture)
	 ("C-c a"  . org-agenda)
	 ("C-c l"  . org-store-link))
  :config (progn
	    (setq org-directory "~/org")
	    (setq org-agenda-files
		  (mapcar (lambda (path) (concat org-directory path))
			  '("/gtd.org")))
            (setq org-log-done 'time)
	    (setq org-src-fontify-natively t)
	    (setq org-use-fast-todo-selection t)
	    (setq org-todo-keywords
		  '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
	    (setq org-capture-templates
		  '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
		     "* TODO %?\n")
		    ("i" "Idea" entry (file+headline (concat org-directory "/gtd.org") "Ideas")
		     "* %? :IDEA: \n%t")
		    ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
		     "* %?\nEntered on %U\n  %i\n ")
			("n" "Note" entry (file (concat org-directory "/notes.org"))
		     "* %?\n")
		    ("l" "Links" item (file+headline (concat org-directory "/links.org") "Temporary Links")
		     "%?\nEntered on %U\n %a")))))

;; Smartparens
(use-package smartparens
  :ensure t
  :config (progn
	    (show-smartparens-global-mode +1)
	    (smartparens-global-mode 1)
	    (smartparens-strict-mode)
	    (setq sp-base-key-bindings 'paredit)
	    (sp-with-modes sp-lisp-modes
	      ;; disable ', it's the quote character!
	      (sp-local-pair "'" nil :actions nil)
	      ;; also only use the pseudo-quote inside strings where it serve as
	      ;; hyperlink.
	      (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
	      (sp-local-pair "`" nil
			     :skip-match (lambda (ms mb me)
					   (cond
					    ((equal ms "'")
					     (or (sp--org-skip-markup ms mb me)
						 (not (sp-point-in-string-or-comment))))
					    (t (not (sp-point-in-string-or-comment)))))))))

;; Avy
(use-package avy
  :ensure t
  :bind (("C-j"   . avy-goto-word-1)
	 ("C-S-j" . avy-goto-line)))

;; Wsbutler
(use-package ws-butler
  :ensure t
  :diminish
  :config (add-hook 'c-mode-common-hook 'ws-butler-mode))

;; Dtrt indent
(use-package dtrt-indent
  :ensure t
  :diminish
  :config (dtrt-indent-mode 1))

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
  :diminish
  :config (progn
	    (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h")
		  guide-key/idle-delay 0.4
		  guide-key/recursive-key-sequence-flag t
		  guide-key/popup-window-position 'right)
	    (guide-key-mode 1)))
