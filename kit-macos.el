;; Settings for GNU emacs 23, cocoa build =======================================
;; ==============================================================================

;; use in server mode
(server-start)

;; this updates the environment based on what specified in the plist
;; rather than the path variable (when you call from Finder, PATH is not set)
;(require 'osx-plist)
;(osx-plist-update-environment)

;; OR a better alternative: if you spend most of your time in Terminal.app and
;; would like emacs to inherit your shell’s environment, open
;; /Applications/Emacs.app will accomplish this.

;; Set the default font 
;; (here b/c I may not have Inconsolata on non-Mac machines)
(set-face-attribute 'default nil :font "Inconsolata 12")

;;to use option as meta in emacs23 (obsolete)
;;(setq mac-option-modifier 'meta)

(setq use-dialog-box nil)

;; ---------------- Trash
(setq delete-by-moving-to-trash t)





;; if you are using the mouse, make sure minibuffer closes what it was doing
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


;; ---------------- features in the latest versions
(if (eq emacs-major-version 23)
    (progn
      (global-visual-line-mode 1) ;;new word wrapping
      (recentf-mode 1) ;;recent files menu
      ))

;; ---------------- having windows we use them
;;Display various non-editing buffers in their own frames
(setq special-display-buffer-names
      (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
  	     special-display-buffer-names))

;;Display those special buffer frames without a tool bar
(add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))

;; ===========================================================================
(provide 'kit-macos)
