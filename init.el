;;; init.el --- Initilization file for Emacs

;;; Commentary:
;;
;; Ducky's clusterf**k of configurations and tweaks.
;; 

;;; Code:
;;;; Package Initilization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;; Check if use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t)
  (setq-default use-package-always-defer t
                use-package-always-ensure t))

;;;; Move customization options out of init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;; (In)sane Defaults
(setq-default delete-by-moving-to-trash t         ; Move files to trash. Don't delete!
	      fill-column 80		                  ; Width for automatic line breaks
	      help-window-select t	                  ; You asked for help, so we'll give to you™!
	      initial-scratch-message ""              ; Empty *scratch* buffer
		  line-spacing 2						  ; Things look less cluttered
	      recenter-positions '(5 top bottom)      ; Modify recentering positions
	      scroll-conservatively 101               ; Add margin for scrolling vertically
	      tab-width 4                             ; I dare you set this to 2. I double dare you.
	      uniquify-buffer-name-style 'forward     ; Uniquify buffer names
	      warning-minimum-level                   ; "A warning? I'll just pretend it's not there..."
	      x-stretch-cursor                        ; Stretch cursor to glyph width
		  x-underline-at-descent-line t			  ; Makes underlines look better
		  )

(blink-cursor-mode 1)			; "Where the hell is my cur...oh there it is"
(electric-pair-mode 1)			; "You complete me..."
(delete-selection-mode 1)		; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)	; Typing yes/no is very annoying
(global-subword-mode 1)			; Iterate through CamelCase words
(global-auto-revert-mode 1)		; Keep buffers up to date
(menu-bar-mode -1)				; *ew* Ugly Menu Bar
(toggle-scroll-bar -1) 			; *ew* Ugly Scroll Bar
(tool-bar-mode -1)				; *ew* Ugly Tool Bar

;;;; Styling
;;;;; Font
(set-frame-font "Roboto Mono Light 12" nil t)
;;;;; Theme
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-plain t))

;;;;; Window
;; Straight up copied from Rougier's Elegant-Emacs
;; https://github.com/rougier/elegant-emacs

(fringe-mode '(0 . 0))

(set-frame-parameter (selected-frame)
					 'internal-border-width 24)

(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

(set-fontset-font "fontset-default"  '(#x2600 . #x26ff) "Fira Code 16")

(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default mode-line-format
     '((:eval
       (mode-line-render
       (format-mode-line (list
         (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                         'help-echo "Mode(s) menu"
                         'mouse-face 'mode-line-highlight
                         'local-map   mode-line-major-mode-keymap)
         " %b "
         (if (and buffer-file-name (buffer-modified-p))
             (propertize "(modified)" 'face `(:inherit face-faded)))))
       (format-mode-line
        (propertize "%4l:%2c  " 'face `(:inherit face-faded)))))))

(defun set-modeline-faces ()

  ;; Mode line at top
  (set-face 'header-line                                 'face-strong)
  (set-face-attribute 'header-line nil
                                :underline (face-foreground 'default))
  (set-face-attribute 'mode-line nil
                      :height 10
                      :underline (face-foreground 'default)
                      :overline nil
                      :box nil 
                      :foreground (face-background 'default)
                      :background (face-background 'default))
  (set-face 'mode-line-inactive                            'mode-line)
  
  ;; Mode line at bottom
  ;; (set-face 'header-line                                 'face-strong)
  ;; (set-face-attribute 'mode-line nil
  ;;                     :height 1.0
  ;;                     :overline (face-background 'default)
  ;;                     :underline nil
  ;;                     :foreground (face-foreground 'default)
  ;;                     :background (face-background 'face-subtle)
  ;;                     :box `(:line-width 2
  ;;                            :color ,(face-background 'face-subtle)
  ;;                            :style nil))
  ;; (set-face 'mode-line-highlight '(face-popout mode-line))
  ;; (set-face 'mode-line-emphasis  'face-strong)
  ;; (set-face-attribute 'mode-line-buffer-id nil :weight 'regular)
  ;; (set-face-attribute 'mode-line-inactive nil
  ;;                     :height 1.0
  ;;                     :overline (face-background 'default)
  ;;                     :underline nil
  ;;                     :foreground (face-foreground 'face-faded)
  ;;                     :background (face-background 'face-subtle)
  ;;                     :box `(:line-width 2
  ;;                            :color ,(face-background 'face-subtle)
  ;;                            :style nil))


  (set-face-attribute 'cursor nil
                      :background (face-foreground 'default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'mode-line))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground (face-background 'default))
  )

;; Comment if you want to keep the modeline at the bottom
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format'(""))

;;;; Mode Packages
;;;;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	     ("\\.md\\'" . markdown-mode)
	     ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;; Ledger Mode
(use-package ledger-mode
  :ensure t
  :hook (ledger-mode . (lambda ()
                         (setq-local tab-always-indent 'complete)
                         (setq-local completion-cycle-threshold t)
                         (setq-local ledger-complete-in-steps t))))

;;;;; Org Mode
;;;;;; General
(use-package org
  :config (setq org-hide-emphasis-markers t ; Org headings look much more cleaner
				org-log-done 'time			; Show when you finished a task
				org-log-done 'note			; Prompt a note for when you finish a task
				org-startup-indented t))	; View org document as indented

;;;;;; Agenda
(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :config (setq org-agenda-span 14
				org-agenda-todo-ignore-timestamp t
				org-agenda-prefix-format		    ; Property tags are often too long to the
				'((agenda . " %i  %-30:c%?-12t% s") ; point where they misalign the agenda
				  (todo . " %i %-12:c")				; view.
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
  :hook (dired-mode . org-download-enable))

;;;;; Outshine Mode
(use-package outshine
  :ensure t
  :hook (emacs-lisp-mode . outshine-mode))

;;;; Misc Packages
;;;;; Company
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config (setq company-idle-delay 0.1))

;;;;; Dashboard
(use-package dashboard
  :ensure t
  :init (dashboard-setup-startup-hook))

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

;;;;; Visual Fill Column
(use-package visual-fill-column
  :ensure t
  :hook ((text-mode . turn-on-visual-line-mode)
         (visual-line-mode . visual-fill-column-mode))
  :config (setq visual-line-mode t
				visual-fill-column-width 80))

(provide 'init)
;;; init.el ends here
