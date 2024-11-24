;;; style.el --- Setting how emacs looks

;;; Commentary:
;; Configuration for setting what Emacs looks like
;;

;;; Code:
;;;; Window

(straight-use-package
 '(nano :type git :host github :repo "rougier/nano-emacs"))
(require 'nano-base-colors)
(require 'nano-faces)
(require 'nano-layout)
(require 'nano-theme)
(require 'nano-modeline)

(add-hook 'after-init-hook (lambda () (nano-refresh-theme)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq display-line-numbers-type nil)

;;;; Themes
;; (straight-use-package 'doom-themes)

;;;; Modeline
;; (straight-use-package 'doom-modeline)
;; (add-hook 'after-init-hook #'doom-modeline-mode)
;; (setq doom-modeline-height 25)

;;;; Beacon
(straight-use-package 'beacon)
(beacon-mode)

;;; style.el ends here
