;;; style.el --- Setting how emacs looks

;;; Commentary:
;; Configuration for setting what Emacs looks like
;;

;;; Code:
;;;; Window
;; (defun init-style ()
;;   "Contain styles."
;;   (setq left-margin-width 2)		; Left margin
;;   (setq right-margin-width 2)		; Right margin
;;   (setq header-line-format " ")		; Set margin as just empty
;;   (set-face-attribute 'header-line nil	; Top margin height
;; 		      :height 200)
;;   )
;; (add-hook 'emacs-startup-hook (init-style))

;;;; Themes
(straight-use-package 'doom-themes)

;;;; Modeline
(straight-use-package 'doom-modeline)
(add-hook 'after-init-hook #'doom-modeline-mode)
(setq doom-modeline-height 25)

;;;; Beacon
(straight-use-package 'beacon)
(beacon-mode)

;;; style.el ends here
