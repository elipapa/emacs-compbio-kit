(require 'python-mode)

;; completion with Rope
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(eval-after-load 'auto-complete
  '(progn
     (ac-ropemacs-initialize)
     (add-hook 'python-mode-hook
               (lambda ()
                 (add-to-list 'ac-sources 'ac-source-ropemacs)))))



;; completion with pysmell
(defvar ac-source-pysmell
  '((candidates
     . (lambda ()
         (require 'pysmell)
         (pysmell-get-all-completions))))
  "Source for PySmell")

(add-hook 'python-mode-hook
          '(lambda ()             
             (set (make-local-variable 'ac-sources) (append ac-sources '(ac-source-pysmell)))))


;; Pylookup
(define-key py-mode-map (kbd "C-c h") 'pylookup-lookup)

;; Flymake
(eval-after-load 'flymake
  '(progn
     (defun flymake-pylint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "epylint" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init))))


;; ============================
(provide kit-python)
