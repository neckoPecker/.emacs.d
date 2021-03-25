;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables

;;; General Emacs
(setq user-backup-directory '(("~/.emacs.d/FileBackups/")))
(setq user-default-directory "~/")

;;; Dashboard
(setq dashboard-banner-logo-title "Welcome...")
(setq dashboard-banner-logo-png "~/.emacs.d/images/dark_emacs.png")
(setq dashboard-startup-banner "~/.emacs.d/images/dark_emacs.png")

;; Org
(setq user-agenda-list '("~/Documents/Personal/Tasks.org"))
(setq user-tasks-file "~/Documents/Personal/Tasks.org")
(setq user-journals-file "~/Documents/Personal/Journals.org")
(setq user-prompts-file "~/Documents/Personal/Journals.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Configuraiton

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Configuration

;;; Essentials
(global-auto-revert-mode 1)
(global-display-line-numbers-mode)
(electric-pair-mode 1)
(setq backup-directory-alist user-backup-directory)
(setq default-directory user-default-directory)

;;; Bar Extensions
(display-time-mode 1)

;;; Useless
(setq inhibit-startup-message t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; Font
(set-frame-font "DejaVu Sans Mono 11" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions

(defun edit-config ()
  "Edit the configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Configuration

;;; General Packages
(use-package company 
  :ensure
  :hook (after-init . global-company-mode)
  :config (setq company-idle-delay 0.1))


(use-package dashboard
  :ensure
  :init (dashboard-setup-startup-hook))


(use-package deft
  :ensure
  :bind ("<f8>" . deft)
  :init
  (setq deft-text-mode 'org-mode
	deft-extensions '("org")
	deft-recursive t))


(use-package doom-themes
  :ensure
  :init (load-theme 'doom-plain t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package ivy
  :ensure
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t))


(use-package ledger-mode
  :ensure
  :hook (ledger-mode . (lambda ()
		       (setq-local tab-always-indent 'complete)
		       (setq-local completion-cycle-threshold t)
		       (setq-local ledger-complete-in-steps t))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package org
  :bind (("C-c  l" . org-store-link))
  :config (setq org-hide-emphasis-markers t
		org-log-done 'time
		org-log-donw 'note
		org-startup-indented t))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files user-agenda-list)
  (setq org-agenda-span 14
	org-agenda-todo-ignore-tempstamp t))


(use-package org-capture
  :bind ("C-c c" . org-capture)
  :config (setq org-capture-templates
		'(("t" "Task Entry" entry (file+headline user-tasks-file)
		   "* TODO %\n %U")
		  ("j" "Journal Entry" entry (file+datetree user-journals-file)
		   "* Entered on %U\n %?")
		  ("p" "Prompt Entry" entry (file+datetree user-prompts-file)
		   "* Entered on %U%?"))))


(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))


(use-package org-cliplink
  :ensure
  :bind (("C-x p i" . org-cliplink)))


(use-package org-download
  :ensure
  :hook (dired-mode . org-download-enable)
  :config (org-download-screenshot "flameshot gui --raw > %s"))


(use-package visual-fill-column
  :ensure
  :hook ((text-mode . turn-on-visual-line-mode)
	 (visual-line-mode . visual-fill-column-mode))
  :config (setq fill-column 100
		visual-line-mode t))

;;; Programming

(use-package lsp-mode :ensure t)
(use-package lsp-java :hook (java-mode . lsp))
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)
(use-package dap-mode :ensure t :after lsp-mode :config (dap-auto-configure-mode))
(use-package yasnippet :ensure t :config (yas-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
