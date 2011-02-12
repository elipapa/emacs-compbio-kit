(when (require 'diminish nil 'noerror)
(eval-after-load "abbrev"
    '(diminish 'autopair-mode "P"))
  (eval-after-load "yasnippet"
    '(diminish 'yas/minor-mode "Y")))

(add-hook 'emacs-lisp-mode-hook 
  (lambda()
    (setq mode-name "el"))) 

(provide kit-misc)
