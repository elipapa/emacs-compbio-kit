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
(add-to-list 'load-path dotfiles-dir)

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

        ess
        org-mode
        pymacs
        ropemacs
        ))


(when window-system
   (add-to-list 'el-get-sources  'color-theme-tango)
   (add-to-list 'el-get-sources  'naquadah-theme))


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
(require 'kit-python)
(require 'kit-misc)

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
