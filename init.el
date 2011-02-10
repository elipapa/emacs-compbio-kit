

;; set root directory
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up el-get, the package manager
;; (notice the double nested structure is necessary to keep the packages in order)
(add-to-list 'load-path (concat dotfiles-dir "/el-get/el-get"))
(require 'el-get)

(setq el-get-sources
      '(el-get
        switch-window
        vkill
        nxhtml
        yasnippet
	auto-complete
        org-mode

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
	ess
	org-mode
	))


(when window-system
   (add-to-list 'el-get-sources  'color-theme-tango))


(el-get)

