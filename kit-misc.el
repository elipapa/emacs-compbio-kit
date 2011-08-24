(add-hook 'emacs-lisp-mode-hook 
  (lambda()
    (setq mode-name "el"))) 


(add-to-list 'safe-local-variable-values '(lexical-binding . t))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; edit as sudo
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; nxhtml stuff
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil)


;; text editing
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook 'turn-on-flyspell) ;;too slow

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))


;; Cosmetics
(setq font-lock-maximum-decoration ;;make dired less colorful 
      '((dired-mode . default) (t . t))) ;; but make the rest as colorful as possible

;; color-theme should handle these next two
;; (eval-after-load 'diff-mode
;;   '(progn
;;      (set-face-foreground 'diff-added "green4")
;;      (set-face-foreground 'diff-removed "red3")))

;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "green3")
;;      (set-face-foreground 'magit-diff-del "red3")
;;      (when (not window-system)
;;        (set-face-background 'magit-item-highlight "white"))))


;; enable narrowing (Restrict editing in this buffer to the current region)
(put 'narrow-to-region 'disabled nil)


;=====================
(provide 'kit-misc)
