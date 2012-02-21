(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; style information?
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;;http://www.gnu.org/software/emacs/manual/html_node/reftex/Multifile-Documents.html
(setq-default TeX-master nil) ;;AucTex prompts for master file for any new file

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; output default to PDF
(setq TeX-PDF-mode t)

;; xelatex together with fontspec allows to access and to use the native fonts
;; of your system
(setq TeX-engine 'xetex)



;; link Skim with AucTeX
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)

(add-hook 'LaTeX-mode-hook
      (lambda()
        (add-to-list 'TeX-expand-list
             '("%q" skim-make-url))))

(defun skim-make-url () (concat
        (TeX-current-line)
        " "
        (expand-file-name (funcall file (TeX-output-extension) t)
            (file-name-directory (TeX-master-file)))
        " "
        (buffer-file-name)))

(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))

(setq TeX-view-program-selection '((output-pdf "Skim")))

;; keybinding for Ebib
(add-hook 'LaTeX-mode-hook #'(lambda ()
          (local-set-key "\C-cb" 'ebib-insert-bibtex-key)))
(global-set-key "\C-ce" 'ebib)

;; ;; add XeTeX as one of the commands
;; (add-to-list 'TeX-command-list
;;              '("XeLaTeX_SyncteX" "%`xelatex --synctex=1%(mode)%' %t"
;;                TeX-run-TeX nil (latex-mode doctex-mode) :help "Run XeLaTeX") t)
;; may not find this necessary, given that AuCTex now has an option to choose
;; the compiler in the dropdown menu


(provide 'kit-latex)
