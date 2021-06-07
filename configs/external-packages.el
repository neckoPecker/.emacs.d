;;; packages.el --- External packages to be used

;;; Commentary:
;; External packages to be used.
;; "Simple packages" are packages that do not require much configuration to work.
;; "Complicated packages" are packages that have a lot of settings to tinker with.
;; Often, it's just packages that have a lot of other packages built on top of them.

;;; Code:
;;;; Simple packages
;;;;; Auctex
(use-package tex
  :ensure auctex)

;;;;; Company
(use-package company
  :ensure t
  :hook (emacs-startup . global-company-mode)
  :config (setq company-idle-delay 0.1))

;;;;; Csharp Mode
(use-package csharp-mode
  :ensure t)

;;;;; CMake Mode
(use-package cmake-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("/CMakeLists.txt\\'" . cmake-mode) t))

;;;;; Dash
(use-package dash
  :ensure t)

;;;;; Dashboard
(use-package dashboard
  :ensure t
  :init (dashboard-setup-startup-hook)
  :config (setq dashboard-items '((recents . 5)
				  (bookmarks . 5)
				  (projects . 5)
				  (agenda . 5)
				  (registers . 5))))

;;;;; Dap
(use-package dap-mode
  :ensure t)

;;;;; Deft
(use-package deft
  :ensure t
  :bind ("C-c <f8>" . deft)
  :init (setq deft-text-mode 'org-mode
			  deft-extensions '("org")
			  deft-recursive t
			  deft-new-file-format "%Y-%m-%dT%H%M"))
;;;;; Flycheck
(use-package flycheck
  :ensure t
  :hook (text-mode . flyspell-mode)
  :init (global-flycheck-mode))

;;;;; Ivy
(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :config (setq ivy-use-virtual-buffers t
		enable-recursive-minibuffers t))

;;;;; Ledger Mode
(use-package ledger-mode
  :ensure t
  :hook (ledger-mode . (lambda ()
                         (setq-local tab-always-indent 'complete)
                         (setq-local completion-cycle-threshold t)
                         (setq-local ledger-complete-in-steps t))))

;;;;; Lua Mode
(use-package lua-mode
  :ensure t)



;;;;; Magit
(use-package magit
  :ensure t)

;;;;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	     ("\\.md\\'" . markdown-mode)
	     ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;; Origami
(use-package origami
  :ensure t)

;;;;; Outshine Mode
(use-package outshine
  :ensure t
  :hook (emacs-lisp-mode . outshine-mode))

;;;;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind ("C-c p" . projectile-command-map))

;;;;; S
(use-package s
  :ensure t)

;;;;; Slime
(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "/bin/sbcl"))

;;;;; Visual Fill Column
(use-package visual-fill-column
  :ensure t
  :hook ((text-mode . turn-on-visual-line-mode)
         (visual-line-mode . visual-fill-column-mode))
  :config (setq visual-line-mode t
		visual-fill-column-width 120))

;;;; Complicated packages
;;;; Math preview
;; Note that you need to install the node.js program.
;; Consult https://gitlab.com/matsievskiysv/math-preview.
(use-package math-preview
  :ensure t)

;;;;; Lsp
;;;;;; Lsp General
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l"))

;;;;;; Lsp Ivy
(use-package lsp-ivy
  :ensure t)

;;;;;; Lsp Treemacs
(use-package lsp-treemacs
  :ensure t
  :config (lsp-treemacs-sync-mode 1))

;;;;;; Lsp UI
(use-package lsp-ui
  :ensure t
  :config (setq lsp-ui-sideline-show-diagnostics t
		lsp-ui-show-hover t
		lsp-ui-sideline-update-mode t))

;;;;;; Lsp Java
(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp))

;;;;; Org Mode
;;;;;; General
(use-package org
  :config (setq org-hide-emphasis-markers t 		; Org headings look much more cleaner
		org-log-done 'time 			; Show when you finished a task
		org-log-done 'note 			; Prompt a note for when you finish a task
		org-startup-indented t)			; View org document as indented
  (plist-put org-format-latex-options :scale 1.5) 	; Increase the size of latex expressions/equations
  (set-face-attribute 'org-headline-done nil :strike-through t)) ; Strikethrough DONE tasks (it's very motivating!)

;; Automatically update org-mode file date
(setq time-stamp-pattern "8/#\\+date:[ \t]+\\\\?[\"<]+%:y-%02m-%02d\\\\?[\">]")
(add-hook 'before-save-hook 'time-stamp)

;; TODO Dependencies
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)

;; Skip agenda schedule if done
(setq org-agenda-skip-scheduled-if-done t)

;;;;;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (python . t)
   (java . t)
   (shell . t)))

;;;;;; Agenda
(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :config (setq org-agenda-span 14
				org-agenda-todo-ignore-timestamp t
				org-agenda-prefix-format	    ; Property tags are often too long to the
				'((agenda . " %i  %-30:c%?-12t% s") ; point where they misalign the agenda
				  (todo . " %i %-12:c")		    ; view.
				  (tags . " %i %-12:c")
				  (search . " %i %-12:c"))))

;;;;;; Capture
(use-package org-capture
  :bind ("C-c c" . org-capture))

;;;;;; Cliplink
(use-package org-cliplink
  :ensure t
  :bind (("C-x p i " . org-cliplink)))

;;;;;; Download
(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable)
  :init (with-eval-after-load 'org (org-download-enable))
  :config (setq-default org-download-image-dir "./org-images/"))

;;;;;; Roam
(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :bind (("C-c n l" . org-roam)
		 ("C-c n f" . org-roam-find-file)
		 ("C-c n g" . org-roam-graph)
		 ("C-c n i" . org-roam-insert)
		 ("C-c n I" . org-rooam-insert-immediate)))

;;;;;; Roam Server
(use-package org-roam-server
  :ensure t
  :config (setq org-roam-server-host "127.0.0.1"
				org-roam-server-port 8080
				org-roam-server-authenticate nil
				org-roam-server-export-inline-images t
				org-roam-server-serve-files nil
				org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
				org-roam-server-network-poll t
				org-roam-server-network-arrows nil
				org-roam-server-network-label-truncate t
				org-roam-server-network-label-truncate-length 60
				org-roam-server-network-label-wrap-length 20))
;;;;; Yasnippet
;;;;;; Package
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

;;;;;; Snippets
(use-package yasnippet-snippets
  :ensure t)

(provide 'init)
;;; packages.el ends here
