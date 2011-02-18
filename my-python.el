;; python stuff only ===============================================

;(require 'python-mode)
(require 'python) ;the order ends up mattering for ipython to define py-shell
;; I would use the normal python.el mode provided by emacs but it appears that
;; for a lot of packages to work python-mode.el is necessary


;; ------------------------------------- add all the commands to path
;(setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":~/.emacs.d/pymodules"))
;(setq exec-path (append exec-path '("~/.emacs.d/pymodules")))

;; -------------------------------------- pyMACS, use python instead of lisp
;; https://github.com/pinard/Pymacs

;; Run Pymacs using the Python virtualenv we build under .emacs.d.
(setenv "PYMACS_PYTHON" "~/.emacs.d/pyenv/bin/python")

(add-to-list 'load-path "~/.emacs.d/pyenv/Pymacs-0.23") ;where pymacs.el is
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;; If you plan to use a special directory to hold your own Pymacs code in
;; Python, which should be searched prior to the usual Python import search
;; path, then uncomment and replace YOUR-PYMACS-DIRECTORY


(add-hook 'before-save-hook 'whitespace-cleanup)


;; ------------------------------------- rope, py refactoring library
;; using the rope + ropemacs installed in the virtualenv
(pymacs-load "ropemacs" "rope-") ;;slow!!

;; TODO FUTURE in the future, when I know Rope better and I don't need the top
;; menu, we can just use a function to autoload it when needed:
(defun load-ropemacs ()
    "Load pymacs and ropemacs"
    (interactive)
    ;; the following autoloads pymacs, with the definitions above
    (pymacs-load "ropemacs" "rope-")
    ;; Automatically save project python buffers before refactorings
    (setq ropemacs-confirm-saving 'nil)
    )
;; (global-set-key "\C-xpl" 'load-ropemacs)


;; ------------------------------------- Yasnippet, textmate-like snippets!
;;normal install, since I want to make my own snippets later on..
(add-to-list 'load-path
                  "~/.emacs.d/lisp/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/mysnippets")


;; ------------------------------------ CEDET
;; AutoComplete.el needs it to display the dropdrown menu but
;; only if in graphic mode
(if (equal window-system 'ns)
    (load-file "~/.emacs.d/lisp/cedet-1.0pre7/common/cedet.el"))
(semantic-load-enable-minimum-features)
;; if more features are needed:
;(semantic-load-enable-excessive-code-helpers)


;; ------------------------------------- Autocomplete
(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete-1.2/")
(when (require 'auto-complete-config nil 'noerror) ;; don't break if not installed
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete-1.2/ac-dict")
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (ac-config-default)
  )

;start when more than 4 characters
(setq ac-auto-start 2)
;delay showing dropdown menu by setting to a number (seconds). nil for no menu
(setq ac-auto-show-menu 1.0)
;more options at http://cx4a.org/software/auto-complete/manual.html#Tips

;(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil) ; return just exits
;(define-key ac-completing-map (kbd "<down>") nil)
;(define-key ac-completing-map (kbd "<up>") nil)
(setq ac-use-quick-help nil)

;; (defvar ac-source-rope
;;   '((candidates
;;      . (lambda ()
;;          (prefix-list-elements (rope-completions) ac-target))))
;;   "Source for Rope")

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'ac-sources)
;;                  (append ac-sources '(ac-source-rope)))))



;;; -------------- BROKEN -------------- Auto-completion
;;;  Integrates: 1) Rope 2) Yasnippet all with AutoComplete.el
;;(require 'my-python-autocompl)

;; TODO : need to make sure I am using the virtualenv environment

;; ------------------------- pysmell
(require 'pysmell)
(add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))


;; ------------------------------------- code checking via flymake
;; set code checker here from "epylint", or could use "pyflakes"
(setq pycodechecker "~/.emacs.d/pyenv/bin/pyflakes")

(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

;; ------------------------------------- more codechecking
;; this needs pep8.py in the path
(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")
(autoload 'pylint "python-pylint")


;; ------------------------------------- ipython

;; specify ipython if the normal 'ipython' doesn't work on CLI
(setq ipython-command "~/.emacs.d/pyenv/bin/ipython")
(require 'ipython)
(setq py-python-command-args '("--colors" "LightBG"))
; possibly better on dark background is the following
;(setq py-python-command-args '("--pylab" "--colors" "Linux"))
(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")



;; activate ElDoc, even though I haven't found out how/if it works
(add-hook 'python-mode-hook
          '(lambda () (eldoc-mode 1)) t)

;; ------------------------------------- activate rst-mode
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;; ------------------------------------- set keys for Modes
;; (global-set-key [f8]      'flymake-mode)
;; (global-set-key [f9]      'python-mode)
;; (global-set-key [f10]      'doctest-mode)
;; (global-set-key [f11]      'rst-mode)


;; ;; ------------------------- Electric Pairs
;; --- not working ---
;; (defun electric-pair ()
;;   "If at end of line, insert character pair without surrounding spaces.
;;     Otherwise, just insert the typed character."
;;   (interactive)
;;   (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))
;; (add-hook 'python-mode-hook
;;      (lambda ()
;;       (define-key python-mode-map "\"" 'electric-pair)
;;       (define-key python-mode-map "\'" 'electric-pair)
;;       (define-key python-mode-map "(" 'electric-pair)
;;       (define-key python-mode-map "[" 'electric-pair)
;;       (define-key python-mode-map "{" 'electric-pair)))

;; (defun electric-pair ()
;;   "Insert character pair without sournding spaces"
;;   (interactive)
;;   (let (parens-require-spaces)
;;     (insert-pair)))


;; Autofill inside of comments
;; this unfortunately does not work in docstrings
;; (defun python-auto-fill-comments-only ()
;;   (auto-fill-mode 1)
;;   (set (make-local-variable 'fill-nobreak-predicate)
;;        (lambda ()
;;          (not (python-in-string/comment)))))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (python-auto-fill-comments-only)))

;; highlight XXX style code tags in source files
(font-lock-add-keywords 'python-mode
  '(("\\<\\(FIXME\\|NOTE\\|HACK\\|XXX\\|TODO\\)" 1 font-lock-warning-face prepend)))


;; ------------------------- more virtualenv stuff
; from http://blog.dreid.org/2010/02/mimicing-source-virtualenvbinactivate.html
;; (defun add-to-PATH (dir)
;;   "Add the specified path element to the Emacs PATH"
;;   (interactive "DEnter directory to be added to PATH: ")
;;   (if (file-directory-p dir)
;;       (setenv "PATH"
;;               (concat (expand-file-name dir)
;;                       path-separator
;;                       (getenv "PATH")))))

;; (defun activate-virtualenv (dir)
;;   (setenv "VIRTUAL_ENV" dir)
;;   (add-to-PATH (concat dir "/bin"))
;;   (add-to-list 'exec-path (concat dir "/bin")))


; check out http://jesselegg.com/archives/2010/03/14/emacs-python-programmers-2-virtualenv-ipython-daemon-mode/
;http://blog.doughellmann.com/search?updated-max=2009-12-10T07%3A33%3A00-05%3A00&max-results=5



;; ------------------------------------ pdb
;; ;;pdb setup, note the python version
;;  (setq pdb-path '~/.emacs.d/pyenv/lib/python2.6/pdb.py
;;        gud-pdb-command-name (symbol-name pdb-path))
;;  (defadvice pdb (before gud-query-cmdline activate)
;;    "Provide a better default command line when called interactively."
;;    (interactive
;;     (list (gud-query-cmdline pdb-path
;;                          (file-name-nondirectory buffer-file-name)))))



;; ------------------------------------ trailing whitespaces
;; FIXME: need to activate a different whitespace mode for python only

;; nuke trailing whitespaces when writing to a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; ;; display only tails of lines longer than 80 columns, tabs and
;; ;; trailing whitespaces
;; (setq whitespace-line-column 80
;;       whitespace-style '(tabs trailing lines-tail))

;; ;; face for long lines' tails
;; (set-face-attribute 'whitespace-line nil
;;                     :background "red1"
;;                     :foreground "yellow"
;;                     :weight 'bold)

;; ;; face for Tabs
;; (set-face-attribute 'whitespace-tab nil
;;                     :background "red1"
;;                     :foreground "yellow"
;;                     :weight 'bold)

;; activate minor whitespace mode when in python mode
(add-hook 'python-mode-hook 'whitespace-mode)


;; ===========================================================================
(provide 'my-python)
