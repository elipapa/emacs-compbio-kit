;;; system specific settings 
;; (location of executables, etc)

(setq default-frame-alist
      '(
        ;; frame width and height
        (width             . 100)
        (height            . 50)
        )
      )

(python-use-ipython "/opt/local/bin/ipython" '("-colors" "LightBG" "-nobanner"))



;; org
;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to <your Dropbox root directory>/MobileOrg.
;(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; Set to the files (or directory of files) you want sync'd
;(setq org-agenda-files (quote ("~/Dropbox/Org")))
;; Set to the name of the file where new notes will be stored
;(setq org-mobile-inbox-for-pull "~/Dropbox/Org/from-mobile.org")
