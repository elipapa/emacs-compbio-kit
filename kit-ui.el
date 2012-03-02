;; themes
;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'tango-dark t)

;; fix the text coloring in the "term" modes
(setq term-default-bg-color nil)
(setq term-default-fg-color nil)


(set-scroll-bar-mode 'nil)

;; scroll while leaving the mark at the same pt - i find it great, some people
;; hate it
;; (setq-default scroll-lock-mode nil) ;setq-default to make it a global minor mode
;; alternatives to this are using one of the "smooth-scrolling" packages

;; (setq
;;   scroll-margin 0                  
;;   scroll-conservatively 100000
;;   scroll-preserve-screen-position 1)

;; nXhtml mode changes the background colors
(setq mumamo-background-colors nil)

(provide 'kit-ui)
