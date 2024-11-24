;;; init.el --- Initilization file for Emacs

;;; Commentary:
;;
;; A startup file that prepares to load all other files required.
;; 

;;; Code:
;;;; Setup

;;;;; Straight.el Initialization
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;; Move customization options out of init.el
(unless (file-exists-p (expand-file-name "custom.el" user-emacs-directory))
  (write-region "" nil (expand-file-name "custom.el" user-emacs-directory)))

(setq custom-file  (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;; Loading the packages
(add-to-list 'load-path "~/.emacs.d/configs/")
(load-library "general")
(load-library "style")
(load-library "external-packages")

;;; init.el ends here
