
;; ===================================== Dired

;; Toggle listing details (ll vs. ls) in dired buffer
;;    this feature has been added to Emacs, starting with Emacs 22.2
;;(require 'dired-details+)
;;(require 'dired-x) ;; not necessary if using dired-details already


;; allows to use 'a' in dired mode so that you don't open a thousand buffers
(put 'dired-find-alternate-file 'disabled nil)

;; allows dired to delete subdirectories too...
;; asks for confirmation at each level!
(setq dired-recursive-deletes 'top)

;; to exclude pyc,backups,etc from dired listings..
;; (require 'dired-x) 
;; (setq dired-omit-files 
;;       (rx (or (seq bol (? ".") "#")         ;; emacs autosave files 
;;               (seq "~" eol)                 ;; backup-files 
;;               (seq bol "svn" eol)           ;; svn dirs 
;;               (seq ".pyc" eol)
;;               ))) 
;; (setq dired-omit-extensions 
;;       (append dired-latex-unclean-extensions 
;;               dired-bibtex-unclean-extensions 
;;               dired-texinfo-unclean-extensions)) 
;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))) 
;; (put 'dired-find-alternate-file 'disabled nil)


;; read info files with 'I'  can be accomplished 
;; with `dired-x.el’ in GNU Emacs
;; (add-hook 'dired-load-hook
;;               (lambda ()
;;                 (load "dired-x"))) ; This also sets `N' to read man pages.
;;    Click on URLs in manual pages
;;   (add-hook 'Man-mode-hook 'goto-address)




;;  ===================================== TRAMP
(setq tramp-default-method "ssh") ;; speeds it up



;; ==============================================================
(provide 'kit-diredtramp)
