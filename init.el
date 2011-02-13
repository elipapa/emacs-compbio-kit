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

;; (notice the double nested structure is necessary to keep the
;; packages in order)
(add-to-list 'load-path (concat dotfiles-dir "el-get/el-get"))
(require 'el-get)


(setq el-get-sources
      '(el-get
        package ;;added only to satisfy a package-user-dir requirement
        switch-window
        vkill
        nxhtml
        auto-complete
        dired+
        dired-details
        browse-kill-ring
        fit-frame
        full-ack
        ipython
        markdown-mode
        python-mode
        rst-mode
        pylookup
        undo-tree
        rainbow-mode      ;;displays strings representing colors with the color
        multi-term
        regex-tool
        csv-mode
        auctex
        processing-mode
        nav
        smooth-scroll
        smooth-scrolling


        (:name yasnippet
               :after (lambda ()
                        (yas/initialize)
                        (add-to-list 'yas/snippet-dirs (concat el-get-dir "yasnippet/snippets"))
                        (add-to-list 'yas/snippet-dirs (concat dotfiles-dir "mysnippets"))
                        (yas/reload-all)))

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

        ;; from specific URLs
        (:name ssh
               :type http
               :url "http://www.splode.com/~friedman/software/emacs-lisp/src/ssh.el"
               :features ssh)
        (:name volatile-highlights
               :type git
               :url "git://github.com/k-talo/volatile-highlights.el.git")
        

        ;; from EmacsWiki
        (:name dired-details+
               :type emacswiki
               :features dired-details+)
        (:name ac-R :type emacswiki)
        (:name column-marker :type emacswiki
               :after (lambda ()
                        (global-set-key [?\C-c ?m] 'column-marker-1)))
        (:name cursor-chg :type emacswiki)

        ;; from ELPA
        (:name kill-ring-search :type elpa)
        (:name asciidoc
               :type elpa
               :after (lambda ()
                        (autoload 'doc-mode "doc-mode" nil t)
                        (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
                        (add-hook 'doc-mode-hook '(lambda ()
                                                    (turn-on-auto-fill)
                                                    (require 'asciidoc)))))

        ;; took out the build instructions from the normal recipe because my TeX
        ;; does not work
        (:name ess
               :type svn
               :url "https://svn.r-project.org/ESS/trunk/"
               :info "doc/info/"
               :load-path ("lisp")
               :features ess-site)

        org-mode
        pymacs
        ropemacs
        ))


(when window-system
   (add-to-list 'el-get-sources  'color-theme-tango)
   (add-to-list 'el-get-sources  'naquadah-theme))


(el-get 'sync)




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
(require 'kit-python)
(require 'kit-misc)

;;(require 'kit-coding)

;; TODO.. fix python dependencies, etc.
;;  (require 'my-python)

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
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))





;; NOTES
;; Some key lisp functions:

;;You can test for a given feature (library) using (require... nil t)
;; or `featurep' if already loaded.

;;You can test for the presence of a specific function using
;;  `fboundp'.

;; You can test for the presence of a specific variable using
;;  `boundp'.
