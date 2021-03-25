;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables

;;; General Emacs
(setq user-backup-directory '(("~/.emacs.d/FileBackups/")))
(setq user-default-directory "~/")

;;; Dashboard
(setq dashboard-banner-logo-title "Welcome...")
(setq dashboard-banner-logo-png "~/Pictures/icons/dark_emacs.png")
(setq dashboard-startup-banner "~/Pictures/icons/dark_emacs.png")

;; Org
(setq user-agenda-list '("~/Documents/Personal/Tasks.org"))
(setq user-tasks-file "~/Documents/Personal/Tasks.org")
(setq user-journals-file "~/Documents/Personal/Journals.org")
(setq user-prompts-file "~/Documents/Personal/Journals.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Configuraiton

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
  (find-file "~/.config/emacs/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Configuration

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
  :hook (org-mode . (lambda () (org-super-star 1) )))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(org-superstar org-download org-cliplink visual-fill-column use-package markdown-mode ledger-mode ivy doom-themes deft dashboard company)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
