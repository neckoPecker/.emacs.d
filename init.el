;;; init.el --- Initilization file for Emacs

;;; Commentary:
;;
;; A startup file that prepares to load all other files required.
;; 

;;; Code:
;;;; Setup
;;;;; Package Initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;; Check if use-package pis installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t)
  (setq-default use-package-always-defer t
                use-package-always-ensure t))

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
