;;; general.el --- General configuration
;;; Commentary:
;; General configuration and modes for Emacs built-in functions.
;; Highly opinionated.
;;

;;; Code:
(setq-default delete-by-moving-to-trash t 			; move files to trash. Don't delete!
			  fill-column 80 			; Width for automatic line breaks
			  help-window-select t 			; You asked for help, so we'll give to you™!
			  inhibit-startup-screen t		; Disable annyoing startup
			  initial-scratch-message "" 		; Empty *scratch* buffer
			  line-spacing 2 			; Things look less cluttered
			  recenter-positions '(5 top bottom) 	; Modify recentering positions
			  scroll-conservatively 101 		; Add margin for scrolling vertically
			  tab-stop-list 4	 		; I dare you set this to 2. I double dare you.
			  uniquify-buffer-name-style 'forward 	; Uniquify buffer names
			  warning-minimum-level 		; "A warning? I'll just pretend it's not there..."
			  x-stretch-cursor			; Stretch cursor to glyph width
			  x-underline-at-descent-line t 	; Makes underlines look better
			  )

(fset 'yes-or-no-p 'y-or-n-p)	; Typing yes/no is very annoying

(blink-cursor-mode 1)		; "Where the hell is my cur...oh there it is"
(electric-pair-mode 1)		; "You complete me..."
(delete-selection-mode 1)	; Replace region when inserting text

(global-display-line-numbers-mode t) ; Number lines are useful.
(global-subword-mode 1)		; Iterate through CamelCase words
(global-auto-revert-mode 1)	; Keep buffers up to date
(menu-bar-mode -1)		; *ew* Ugly Menu Bar
(toggle-scroll-bar -1) 		; *ew* Ugly Scroll Bar
(tool-bar-mode -1)		; *ew* Ugly Tool Bar

;;; general.el ends here
