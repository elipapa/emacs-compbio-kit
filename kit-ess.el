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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sweave


;; Make TeX and RefTex aware of Snw and Rnw files
(setq reftex-file-extensions
      '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))

;;; Define Rnw-mode and make LaTeX aware of it. 
(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Rnw-mode))

;; Linking ESS with AucTex
;; Lets you do 'C-c C-c Sweave' from your Rnw file
(add-hook 'Rnw-mode-hook
 (lambda ()
  (add-to-list 'TeX-expand-list '("%rnw" file "Rnw" t) t)
  (add-to-list 'TeX-command-list
               '("Stangle" "R CMD Stangle %rnw" 
                 TeX-run-command nil (latex-mode) :help "Run Stangle") t)
  (add-to-list 'TeX-command-list
               '("Sweave" "R CMD Sweave %rnw" 
                 TeX-run-command nil (latex-mode) :help "Run Sweave") t)
  (add-to-list 'TeX-command-list
               '("LatexSweave" "%l %(mode) %s"
                 TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
  (setq TeX-command-default "Sweave")))

;;all of the above may be taken care by this new one-liner. will test next time
;;I use sweave
;;(setq ess-swv-plug-into-AUCTeX-p t)


;;; Make shift-enter do a lot in ESS.
;; Use shift-enter to split window & launch R (if not running), execute
;; highlighted region (if R running & area highlighted), or execute
;; current line (and move to next line, skipping comments). Nice. See
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics,
;; FelipeCsaszar. Adapted to split vertically instead of horizontally. 

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
	(delete-other-windows)
	(setq w1 (selected-window))
	(setq w1name (buffer-name))
	(setq w2 (split-window w1 nil t))
	(R)
	(set-window-buffer w2 "*R*")
	(set-window-buffer w1 w1name))))

(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))


(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [C-up] 'comint-previous-input)
             (local-set-key [C-down] 'comint-next-input)))
(add-hook 'Rnw-mode-hook 
          '(lambda() 
             (local-set-key [(shift return)] 'my-ess-eval))) 



;;; Make ESS support cacheSweave() library in R
;; ESS comes with support for Sweave, e.g.,: M-n s M-n P from the Rnw file. But
;; we also want to be able to conveniently use the cacheSweave() library, which
;; uses a different driver. So we need a separate command for that. The
;; following solution comes from
;; [[http://blog.nguyenvq.com/2009/05/14/editingadding-on-to-sweave-features-in-ess/][Vinh
;; Nguyen]] and allows for cachSweave() compilation with M-n w.

  
(defun ess-swv-run-in-R2 (cmd &optional choose-process)
  "Run \\[cmd] on the current .Rnw file.  Utility function not called by user."
  (let* ((rnw-buf (current-buffer)))
    (if choose-process ;; previous behavior
    (ess-force-buffer-current "R process to load into: ")
      ;; else
      (update-ess-process-name-list)
      (cond ((= 0 (length ess-process-name-list))
         (message "no ESS processes running; starting R")
         (sit-for 1); so the user notices before the next msgs/prompt
         (R)
         (set-buffer rnw-buf)
         )
        ((not (string= "R" (ess-make-buffer-current))); e.g. Splus, need R
         (ess-force-buffer-current "R process to load into: "))
       ))

    (save-excursion
      (ess-execute (format "require(tools)")) ;; Make sure tools is loaded.
      (basic-save-buffer); do not Sweave/Stangle old version of file !
      (let* ((sprocess (get-ess-process ess-current-process-name))
         (sbuffer (process-buffer sprocess))
         (rnw-file (buffer-file-name))
         (Rnw-dir (file-name-directory rnw-file))
         (Sw-cmd
          (format
           "local({..od <- getwd(); setwd(%S); %s(%S, cacheSweaveDriver()); setwd(..od) })"
           Rnw-dir cmd rnw-file))
         )
    (message "%s()ing %S" cmd rnw-file)
    (ess-execute Sw-cmd 'buffer nil nil)
    (switch-to-buffer rnw-buf)
    (ess-show-buffer (buffer-name sbuffer) nil)))))


(defun ess-swv-weave2 ()
   "Run Sweave on the current .Rnw file."
   (interactive)
   (ess-swv-run-in-R2 "Sweave"))

(define-key noweb-minor-mode-map "\M-nw" 'ess-swv-weave2)


;;================================
(provide 'kit-ess)
