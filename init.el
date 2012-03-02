;; init.el =================================================================
;; Eli Papa
;;  code taken from various sources including emacs-starter-kit + el-get blog
;; =========================================================================

;; ============================ Enable a backtrace when problems occur
;;(setq debug-on-error t)


;;recursion depth -- increase when in trouble. if you need to raise,
;; perhaps there is an issue with your .emacs files - check that first
(setq max-lisp-eval-depth '10000) ;ESS usually needs a high number
(setq max-specpdl-size '10000)


;; ============================ set root directory
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; separate the 'Customize' preferences
(setq custom-file (concat dotfiles-dir "kit-custom.el"))


;; ============================ Load up el-get, the package manager

;; notice the double nested structure is necessary to keep the
;; packages in order
(add-to-list 'load-path (concat dotfiles-dir "el-get/el-get"))

(unless (require 'el-get nil t) ;; if we don't have it locally, install it from github
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq el-get-sources
      '((:name csv-mode
               :after (lambda ()
                        (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
                        (autoload 'csv-mode "csv-mode"
                          "Major mode for editing comma-separated value files." t)))
        (:name yasnippet
               :after (lambda ()
                        (yas/initialize)
                        (add-to-list 'yas/snippet-dirs (concat el-get-dir "yasnippet/snippets"))
                        (add-to-list 'yas/snippet-dirs (concat dotfiles-dir "mysnippets"))
                        (yas/reload-all)))

        ;; this is now replaced by electric-pair-mode, but need to find a way to
        ;; use that with python
        
        ;; (:name autopair
        ;;        :after (lambda ()
        ;;                 (autopair-global-mode 1)
        ;;                 (setq autopair-autowrap t)
        ;;                 (add-hook 'python-mode-hook
        ;;                           #'(lambda ()
        ;;                               (setq autopair-handle-action-fns
        ;;                                     (list #'autopair-default-handle-action
        ;;                                           #'autopair-python-triple-quote-action))))
        ;;                 ))


        

        ;; from specific URLs
        (:name ssh
               :type http
               :url "http://www.splode.com/~friedman/software/emacs-lisp/src/ssh.el"
               :features ssh)
        (:name volatile-highlights
               :type git
               :url "git://github.com/k-talo/volatile-highlights.el.git")
        (:name monky
               :type git
               :url "git://github.com/ananthakumaran/monky.git")
         
        ;; from EmacsWiki
        (:name ac-R :type emacswiki)
        (:name cursor-chg :type emacswiki)

        ;; from ELPA
	(:name auctex :type elpa)
        ;;(:name css-mode :type elpa)
        
        ;; took out the build instructions from the normal recipe because my TeX
        ;; does not work
        (:name ess
               :type svn
               :url "https://svn.r-project.org/ESS/trunk/"
               :info "doc/info/"
               :load-path ("lisp")
               :features ess-site)

        ))




(setq my-packages
      (append
       (mapcar 'el-get-source-name el-get-sources)
       '(cssh
         dired-details
         asciidoc
         ebib
         org-mode
         vkill
         auto-complete 
         browse-kill-ring
         full-ack
         rst-mode
         pylookup
         undo-tree
         multi-term
         regex-tool
         column-marker
         processing-mode
         nav
         magit
         buffer-move
         markdown-mode
         ;;ruby+rails
         inf-ruby
         ruby-compilation
         ruby-mode
         yaml-mode
         rhtml-mode
         scss-mode
         nxhtml ;;contains zen-coding mode and MuMaMo
         ;;TODO: ISSUE scrolling
         smooth-scroll;; works on page movemens C-v/M-v
         smooth-scrolling ;;works only on C-n/C-p one line moves
         ;; these two cannot be used at the same time as

         ;;TODO: ISSUE python
         ;;python-mode
         python
         ;;ipython ;need to be after python-mode in this list
         ;;ac-python
         )))

(el-get 'sync my-packages)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))


;; ============================ load customizations

(require 'kit-necessities)
(require 'kit-bindings)
(if (eq system-type 'darwin)
    (progn
      (require 'kit-macos)))
(require 'kit-shell-term)
(require 'kit-diredtramp)
(require 'kit-navigation)
(require 'kit-latex)
(require 'kit-coding)
(require 'kit-ess)
(require 'kit-orgmode)
(require 'kit-misc)
(require 'kit-python) ;; TODO.. fix python dependencies, clean up all the python-mode-hooks
(require 'kit-ui)

;; Load the M-x customize file last
(load custom-file 'noerror) ;noerror is there if custom file does not exist


;; -- from Emacs Starter Kit --
;; This is where you should put code that you don't think would be useful to
;; everyone. That will allow you to merge with newer versions of the kit
;; without conflicts.

;; 1. a file named after your user (user-login-name) with the extension ".el"
;; 2. if a directory named after your user exists, it will be added to the
;; load-path, and any elisp files in it will be loaded.
;; 3. a file named after the current hostname ending in ".el" which will allow
;; host-specific configuration.

(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name always has a dot
  (setq system-name (car (split-string system-name "\\."))))

(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))


