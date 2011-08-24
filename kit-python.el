(require 'python-mode)

(add-hook 'python-mode-hook 'run-coding-hook)

;; TODO add one functionality at the time

;; ;; completion with Rope
;; (require 'pymacs)
;; ;(pymacs-load "ropemacs" "rope-") ;FIXME
;; (eval-after-load 'auto-complete
;;   '(progn
;;      (ac-ropemacs-initialize)
;;      (add-hook 'python-mode-hook
;;                (lambda ()
;;                  (add-to-list 'ac-sources 'ac-source-ropemacs)))))

;; ;; Stops from erroring if there's a syntax err
;; (setq ropemacs-codeassist-maxfixes 3)
;; (setq ropemacs-enable-autoimport t)


;; ;; completion with pysmell
;; (defvar ac-source-pysmell
;;   '((candidates
;;      . (lambda ()
;;          (require 'pysmell)
;;          (pysmell-get-all-completions))))
;;   "Source for PySmell")

;; (add-hook 'python-mode-hook
;;           '(lambda ()             
;;              (set (make-local-variable 'ac-sources) (append ac-sources '(ac-source-pysmell)))))


;; ;; Pylookup - look up html help files
;; (define-key py-mode-map (kbd "C-c h") 'pylookup-lookup)

;; ;; Flymake - needs pylint egg installed (which contains epylint.py script)
;; ;; TODO: make the corrector a variable, so you can switc between epylint,
;; ;; pyflakes and pep8
;; ;; (eval-after-load 'flymake
;; ;;   '(progn
;; ;;      (defun flymake-pylint-init ()
;; ;;        (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; ;;                           'flymake-create-temp-inplace))
;; ;;               (local-file (file-relative-name
;; ;;                            temp-file
;; ;;                            (file-name-directory buffer-file-name))))
;; ;;          (list "epylint" (list local-file))))

;; ;;      (add-to-list 'flymake-allowed-file-name-masks
;; ;;                   '("\\.py\\'" flymake-pylint-init))))



;; ;;; From python-based ESK
;; ;; Make a choice of corrector with flymake
;; (setq flymake-enable-pyflakes t)
;; (setq flymake-enable-pylint nil)
;; (setq flymake-enable-pep8 nil)

;; (eval-after-load 'python
;; ;; when loading python (or python-mode?) execute this:
;;   '(progn

;;      ;;==================================================
;;      ;; Virtualenv Commands
;;      ;;==================================================
;;      ;; (autoload 'virtualenv-activate "virtualenv"
;;      ;;   "Activate a Virtual Environment specified by PATH" t)

;;      ;; (autoload 'virtualenv-workon "virtualenv"
;;      ;;   "Activate a Virtual Environment present using virtualenvwrapper" t)
     
;;      ;;==================================================
;;      ;; Flymake for python configuration
;;      ;;===================================================

    
;;      ;; Instructions to add a new checker based on command:
;;      ;;
;;      ;; 1) Write an init function, the flymake-command-setup performs some
;;      ;;    checks and at the end of the option list the filename to process:
;;      ;;
;;      ;;   (defun flymake-newchecker-init ()
;;      ;;      (flymake-command-setup "command" (list "option1" "option2")))
;;      ;;
;;      ;; 2) Use the flymake-add-checker function
;;      ;;
;;      ;;    (flymake-add-checker flymake-newchecker-init)

;;      (require 'tramp)
;;      ;; Utilities that increase legibility and reduce code duplication
;;      (defun current-file-remotep ()
;;        "Tell if the file is remote"
;;        (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))

;;      (defun flymake-create-copy-file ()
;;        "Create a copy local file"
;;        (let* ((temp-file (flymake-init-create-temp-buffer-copy 
;;                           'flymake-create-temp-inplace)))
;;          (file-relative-name 
;;           temp-file 
;;           (file-name-directory buffer-file-name))))

;;      (defun flymake-command-setup (command &optional options)
;;        "Setup the command to be used with flymake, the command
;; will be called in this way: COMMAND OPTIONS FILE The FILE varible
;; is passed after the options."
;;        ;; Make sure it's not a remote buffer or flymake would not work
;;        (when (not (current-file-remotep)) 
;;          (list command
;;                (append options (list (flymake-create-copy-file))))))

;;      (when (require 'flymake "flymake-patch" t)
;;        (setq flymake-info-line-regex
;;              (append flymake-info-line-regex '("unused$" "^redefinition" "used$"))))

;;      ;; I'm using individual well-defined names to be able to remove them
;;      ;; in some way

;;      ;; Init functions!
;;      (defun flymake-pyflakes-init ()
;;        (flymake-command-setup "pyflakes"))

;;      (defun flymake-pep8-init ()
;;        (flymake-command-setup "pep8"))

;;      (defun flymake-pylint-init ()
;;        (flymake-command-setup "python" (list (concat dotfiles-dir "scripts/pylint-mod.py"))))

;;      (defun flymake-disable-python-checkers ()
;;        "Disable all python checkers"
;;        (dolist (flymake-checker-init '(flymake-pyflakes-init flymake-pep8-init flymake-pylint-init))
;;          (remove '("\\.py\\'" flymake-checker-init) 'flymake-allowed-file-name-masks)))

;;      (defun flymake-add-checker (command)
;;        "Add the checker specified by the COMMAND list"
;;        (add-to-list 'flymake-allowed-file-name-masks
;;                     (list "\\.py\\'" command)))

;;      ;; Not on all modes, please
;;      (add-hook 'python-mode-hook 'flymake-find-file-hook)

     
;;      (when flymake-enable-pyflakes
;;        (flymake-add-checker 'flymake-pyflakes-init))

;;      (when flymake-enable-pylint
;;        (flymake-add-checker 'flymake-pylint-init))

;;      (when flymake-enable-pep8
;;        (flymake-add-checker 'flymake-pep8-init)))
;;   )






;; facilitate the use of ipython
(defun python-use-ipython (cmd args)
  (setq ipython-command cmd)
  (setq py-python-command-args args)
  (require 'ipython)
  (setq ipython-completion-command-string
        "print(';'.join(__IP.Completer.all_completions('%s')))\n"))


;; ============================
(provide 'kit-python)
