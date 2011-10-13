;; all the settings regarding terminal ===============================
;; this will probably change a lot across platforms
;; ===================================================================

;; ---------------------------------------- load multiterm
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

;; make sure the shell colors are respected (instead of printed as escape sequences)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; use bash
(setq multi-term-program "/bin/bash") 


;;and adjust colors for multi-term... (as explained in the emacswiki)
;; (custom-set-variables
;;      '(term-default-bg-color "Grey15")        ;; background color
;;      '(term-default-fg-color "Grey"))       ;; foreground color



;; TODO  set M-s for cua-mode C-RET, so that it works on terminals

;;============================================================
(provide 'kit-shell-term)
