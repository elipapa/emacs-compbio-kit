;; initial argument in calling R
(setq inferior-R-args "--no-restore-data --quiet") 

;; search history backward, after typing the first few characters
(define-key inferior-ess-mode-map [M-<up>]
	  'comint-previous-matching-input-from-input)

;;;;create a new frame for each help instance
;;;
;;;(setq ess-help-own-frame t)
;;;;;If you want all help
;;;;; buffers to go into one frame do:
;;;
(setq ess-help-own-frame 'one)

;; we'll see how this goes.. it makes the shell feel more like the shell
(setq ansi-color-for-comint-mode 'filter)
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;;(define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
;;(define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)

;; when you eval, don't show the code in the R session (this makes it go much
;; faster)
(setq ess-eval-visibly-p nil)

;; from:
;; http://blogisticreflections.wordpress.com/2009/10/01/r-object-tooltips-in-ess/
;; which has been included in ESS since 5.3
;; uses C-c C-g as the keybindin
(add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key [(C-c C-g)] 'ess-r-args-show)))

;;================================
(provide 'kit-ess)
