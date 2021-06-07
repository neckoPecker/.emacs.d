;;; style.el --- Setting how emacs looks

;;; Commentary:
;; Configuration for setting what Emacs looks like
;;

;;; Code:
;;;; Window
(defun init-style ()
  "Contain styles."
  (setq left-margin-width 2)		; Left margin
  (setq right-margin-width 2)		; Right margin
  (setq header-line-format " ")		; Set margin as just empty
  (set-face-attribute 'header-line nil	; Top margin height
		      :height 200)
  )

(add-hook 'after-init-hook (init-style))

;;;; Themes
(use-package doom-themes
  :ensure t)

;;; style.el ends here
