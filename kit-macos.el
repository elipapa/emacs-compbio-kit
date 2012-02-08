;; Settings for GNU emacs 24, cocoa build =======================================
;; ==============================================================================

;; use in server mode
(server-start)

;; Set the default appearance of all frames 
;; (here b/c I may not have Inconsolata on non-Mac machines)
;;(set-face-attribute 'default nil :font "Inconsolata 12")
(setq default-frame-alist '((font . "Inconsolata 12") (width . 100)))


(setq use-dialog-box nil)

;; Trash
(setq delete-by-moving-to-trash t)


;; if you are using the mouse, make sure minibuffer closes what it was doing
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


;; features in the latest versions
(if (eq emacs-major-version 23)
    (progn
      (global-visual-line-mode 1) ;;new word wrapping
      (recentf-mode 1) ;;recent files menu
      ))

;;Having windows we use them
;;Display various non-editing buffers in their own frames
(setq special-display-buffer-names
      (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
  	     special-display-buffer-names))

;;Display those special buffer frames without a tool bar
(add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))


;; ----------------- spelling (using homebrew aspell)
;; install first on the command line using homebrew:
;; 1brew install aspell --lang=en
(setq-default ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))



;; ===========================================================================
(provide 'kit-macos)
