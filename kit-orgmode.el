;; org mode stuff =================================================

;(require 'org-install) ;;no need to turn this on if org comes with emacs
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-startup-indented t)
(setq org-hide-leading-stars nil)
(setq org-indent-mode-turns-on-hiding-stars nil)
;; from http://orgmode.org/worg/org-faq.php#auto-fill-and-unwanted-comments
(add-hook 'org-mode-hook
          (lambda ()
            (org-set-local 'comment-start nil)))

;; ===============================
(provide 'my-orgmode)