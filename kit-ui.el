;; themes
;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'tango-dark t)

(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

(set-scroll-bar-mode 'nil)

(provide 'kit-ui)
