;; kit-necessities.el =========================================================
;; Fundamental minimum set of changes
;; -  hopefully with the maximum compatibility across platforms/systems
;; -  no external package should be required by this file
;; ============================================================================

;; ---------------------------------------- visual settings
;; these are included early to clean the interface immediately

;; Display line and column numbers
(line-number-mode    1)
(column-number-mode  1)

(setq inhibit-splash-screen t)
(tool-bar-mode -1)

;; i keep the menu bar if i have a windowed system. sometimes it's
;; useful to remember key bindings. 
(if (eq window-system 'nil)
    (progn
      (menu-bar-mode -1)
      ))

;; highlight current editing line
;(global-hl-line-mode) ;try for a bit to do it only in the coding buffers

;;line numbers on the left panel
(global-linum-mode 1)
(setq linum-format "%4d ") ;; make some extra space and keep right justified




;; --------------------------------------- fix autosave and backup

;; Move ~ and # files to the system’s “temp” directory:
;;  *  could be fancier with dated backup, but it's probably best
;;     to use version control anyways.
;;  * can substitute user-temporary-file-directory with temp-file-dir
;;    if one needs a different value than /tmp (unix) or /var/tmp (macos)
;;    (defvar user-temporary-file-directory
;;       (concat temporary-file-directory user-login-name "/"))
;;    (make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,temporary-file-directory)
        (,tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; ---------------------------------------- standards
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; ---------------------------------------- editing + selection

;; use cua-mode for the rectangle support, global mark mode, but leave the C-x
;; C-z C-v keys untouched
(cua-selection-mode t) ;; cua-mode also activates delete-selection-mode automatically

;; make the clipboard work using the x clipboard
(setq x-select-enable-clipboard t)

;; visual feedback when selecting a region
(setq transient-mark-mode t)

;; highlight matching parenthesis when passing over
;; more at http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
(show-paren-mode 1)

;; for some reason this command is disabled-by-default. so we enable it.
(put 'downcase-region 'disabled nil)

;; ---------------------------------------- minor annoyances

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; kill buffers with processes (FIXME)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; Prevent the annoying beep on errors
(setq visible-bell t)





;; ---------------------------------------- philosophical divides

;; force emacs to use spaces instead of tabs (no of spaces should be already
;; specified in modes)
(setq-default indent-tabs-mode nil)

;; make sure files finish in a newline
(setq require-final-newline 't)

;; ---------------------------------------- windows + buffers


;; unique names for buffers (library included in GNU emacs)
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")


;; if a file changes on the disk, update the corresponding buffer
;; (global-auto-revert-mode 1) ;some people like this


;; renaming a file while working on it
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))





;; --------------------------------------- Line-wrapping and Autofill
(setq-default fill-column 80)
;;(setq auto-fill-mode 1)

;(global-visual-line-mode t)
;;; prefer auto-fill to visual line wrap in ESS mode
(add-hook 'ess-mode-hook 'turn-on-auto-fill)
(add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill) 

;;; but turn off auto-fill in tex and markdown
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)

;; wordwrapping.. experiment here with truncate lines
(setq truncate-partial-width-windows nil)
;(setq truncate-lines 80)

;; whitespace-mode
(setq whitespace-style '(trailing lines-tail space-before-tab
                                  tabs tab-mark face indentation
                                  newline-mark newline
                                  space-after-tab))
(setq whitespace-line-column 90)

(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))



;; ------------------------------ faster saves
;; taken from http://www.method-combination.net/blog/archives/2011/03/11/speeding-up-emacs-saves.html
(setq vc-handled-backends nil)


;; ================================================================
(provide 'kit-necessities)
