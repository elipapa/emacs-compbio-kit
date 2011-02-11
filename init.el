;; init.el =================================================================
;; Eli Papa
;;  code taken from various sources including emacs-starter-kit + el-get blog
;; =========================================================================

;; ============================ Enable a backtrace when problems occur
;;(setq debug-on-error t)

;; separate the 'Customize' preferences
(setq custom-file "~/.emacs.d/kit-custom.el")
(load custom-file 'noerror)

;;recursion depth -- increase when in trouble. if you need to raise,
;; perhaps there is an issue with your .emacs files - check that first
(setq max-lisp-eval-depth '10000) ;ESS usually needs a high number
(setq max-specpdl-size '10000)


;; ============================ set root directory
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; ============================ Load up el-get, the package manager

;; (notice the double nested structure is necessary to keep the
;; packages in order)
(add-to-list 'load-path (concat dotfiles-dir "/el-get/el-get"))
(require 'el-get)

(setq el-get-sources
      '(el-get
        switch-window
        vkill
        nxhtml
        yasnippet
	auto-complete
	dired+
	dired-details
        
	(:name autopair
	       :after (lambda ()
			(autopair-global-mode 1)
			(setq autopair-autowrap t)
			(add-hook 'python-mode-hook
				  #'(lambda ()
				      (setq autopair-handle-action-fns
					    (list #'autopair-default-handle-action
						  #'autopair-python-triple-quote-action))))
			))

        (:name buffer-move
               :after (lambda ()
                        (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                        (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                        (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                        (global-set-key (kbd "<C-S-right>")  'buf-move-right)))


        (:name magit
               :after (lambda ()
                        (global-set-key (kbd "C-x C-z") 'magit-status)))

        (:name asciidoc
               :type elpa
               :after (lambda ()
                        (autoload 'doc-mode "doc-mode" nil t)
                        (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
                        (add-hook 'doc-mode-hook '(lambda ()
                                                    (turn-on-auto-fill)
                                                    (require 'asciidoc)))))
	(:name ssh
	       :type url
	       :url http:somethingsomething)

	ess
	org-mode
	))


(when window-system
   (add-to-list 'el-get-sources  'color-theme-tango))


(el-get)




;; ============================ load customizations

(require 'kit-necessities)
(if (equal window-system 'ns)
    (progn
      (require 'kit-macos)))
(require 'kit-shell-term)
(require 'kit-diredtramp)
(require 'kit-navigation)
(require 'kit-ess)
(require 'kit-orgmode)

;;(require 'kit-coding)

;; TODO.. fix python dependencies, etc.
;;  (require 'my-python)


;; --------------------------- smarter redo system (obsolete?)
;;(require 'redo+)




;; NOTES
;; Some key lisp functions:

;;You can test for a given feature (library) using (require... nil t)
;; or `featurep' if already loaded.

;;You can test for the presence of a specific function using
;;  `fboundp'.

;; You can test for the presence of a specific variable using
;;  `boundp'.
