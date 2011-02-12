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
;; useful to remember key bindings. Same goes for the scrollbars
(if (eq window-system 'nil)
    (progn
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      ))

;; highlight current editing line
(global-hl-line-mode)


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


;; ---------------------------------------- editing + selection

;; kill a word by C-w to match the shell
;(global-set-key "\C-w" 'backward-kill-word)

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

;; force emacs to use spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; sets spaces to 4
;;(setq c-basic-indent 2)
;;(setq tab-width 4)
;; NOTE: not turned on because it seems as if python-mode and ESS
;; already operate based on that rule..

;; make sure files finish in a newline
(setq require-final-newline 't)


;; ---------------------------------------- windows + buffers
;; save window configuration history and navigate with C-c <left/right>
(winner-mode 1)

;; helps moving around windows - this does not work in ORG mode
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)


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
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)




;; --------------------------------------- Line-wrapping and Autofill
(setq-default fill-column 80)
;;(setq auto-fill-mode 1)

;; activate autofill for all text mode buffers:
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; To automatically fill comments but not code in ProgrammingModes, something
;; like this can be used:
(add-hook 'c-mode-common-hook
              (lambda ()
                (auto-fill-mode 1)
                (set (make-local-variable 'fill-nobreak-predicate)
                     (lambda ()
                       (not (eq (get-text-property (point) 'face)
                                'font-lock-comment-face))))))





;; --------------------------------------------- registers and rings

;; bind the list-register function, which has been recently included in emacs
(global-set-key (kbd "C-x r v") 'list-registers)

;; Now, the M-y key binding will activate browse-kill-ring iff the last command
;; was not a 'yank'.
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;; C-c y will show a little pop-up menu with the your kill-menu entries.
(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

;; (setq-default kill-ring-max 60)



;; ================================================================
(provide 'kit-necessities)
