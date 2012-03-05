;; org mode stuff =================================================

;;(require 'org-install) ;;no need to turn this on if org comes with emacs

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-completion-use-ido t)
;; (require 'org-special-blocks)
;; (if window-system (require 'org-mouse))

;; Compatibility with WindMove
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
;; (if window-system (require 'org-mouse))



;;The latest version of yasnippets doesn't play well with Org-mode, the following
;;function allows these two to play nicely together
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            ;; table
            (local-set-key "\M-\C-w" 'org-table-copy-region)
            (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
            (local-set-key "\M-\C-l" 'org-table-sort-lines)
            ;; display images
            (local-set-key "\M-I" 'org-toggle-iimage-in-org)
            ;; yasnippet (using the new org-cycle hooks)
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)
            ))

;; Speed commands enable single-letter commands in Org-mode files when
;; the point is at the beginning of a headline, or at the beginning of a
;; code block.

;; See the `=org-speed-commands-default=' variable for a list of the keys
;; and commands enabled at the beginning of headlines.  All code blocks
;; are available at the beginning of a code block, the following key
;; sequence =C-c C-v h= (bound to `=org-babel-describe-bindings=') will
;; display a list of the code blocks commands and their related keys.
(setq org-use-speed-commands t)

;; this activates a number of widely used languages to use in babel code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(setq org-log-done t)
(setq org-startup-indented t)
(setq org-hide-leading-stars nil)
(setq org-indent-mode-turns-on-hiding-stars nil)
;; from http://orgmode.org/worg/org-faq.php#auto-fill-and-unwanted-comments
(add-hook 'org-mode-hook
          (lambda ()
            (org-set-local 'comment-start nil)))

;; ===============================
(provide 'kit-orgmode)
