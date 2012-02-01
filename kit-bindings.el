;;; kit-bindings.el
;;; put all bindings here, so you can reference to them when you forget

;; apple mac os x keybindings
(when (equal window-system 'ns)
  ;; fullscreen
  (global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen)
  ;;top and end of buffer
  (global-set-key (kbd "<s-up>")     'beginning-of-buffer) 
  (global-set-key (kbd "<s-down>")   'end-of-buffer)
  (global-set-key (kbd "<s-left>")   'beginning-of-line-text)
  (global-set-key (kbd "<s-right>")  'end-of-line)

  ;(global-set-key (kbd "<s-left>")   'beginning-of-visual-line)
  ;(global-set-key (kbd "<s-right>")  'end-of-visual-line)
  )


;; kill a word by C-w to match the shell
;(global-set-key "\C-w" 'backward-kill-word)

;;By default C-z is bound to "Suspend Frame", which minimizes Emacs. Bind it to
;;"Undo" instead.
(global-set-key (kbd "C-z") 'undo)


;; save window configuration history and navigate with C-c <left/right>
(winner-mode 1)
(global-set-key (kbd "C-c <up>") 'winner-undo)
(global-set-key (kbd "C-c <down>") 'winner-redo)

;; helps moving around windows - this does not work in ORG mode, but there is a
;; fix in kit-org;
;; use M + arrows to activate the desired window
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; change window splittings (fcns defined in kit-navigation)
;; set the keybindings, 'C-x 4' is where most of the gnu emacs binding for new
;; windows exist
(global-set-key (kbd "C-x 4 t") 'transpose-buffers)
(global-set-key (kbd "C-x 4 h") 'split-window-vertical-to-horizontal)
(global-set-key (kbd "C-x 4 v") 'split-window-horizontal-to-vertical)


;; buffer-move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;; mark and selection with Transient Mark Mode
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer) ;fcn defined in kit-coding

;; Font size - TODO change to Apple bindings
;(define-key global-map (kbd "C-+") 'text-scale-increase)
;(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu) ;fcn defined in kit-navigation

;; File finding
(define-key global-map (kbd "C-x C-f") 'find-file-at-point)

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer) ;very useful! can also turn on auto-revert mode 
(global-set-key (kbd "C-x C-b") 'ibuffer)


(global-set-key (kbd "C-c r") 'rename-file-and-buffer) ;;defined in kit-necessities

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x m") 'shell)

;; Start terminal
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;; If you want to be able to M-x without meta (phones, etc)
;(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
;(global-set-key (kbd "C-c e") 'eval-and-replace) ;need to def fcn

;; very useful! join this line to previous one (and fix whitespace)
(global-set-key (kbd "C-c q") 'join-line)

;;Org
(define-key global-map "\C-cl" 'org-store-link) ;?
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;here for reference...
;; (local-set-key "\M-\C-w" 'org-table-copy-region)
;; (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
;; (local-set-key "\M-\C-l" 'org-table-sort-lines)
;; ;; display images
;; (local-set-key "\M-I" 'org-toggle-iimage-in-org)

;;ESS
;; (local-set-key [(shift return)] 'my-ess-eval)))
;; (local-set-key [C-up] 'comint-previous-input)
;; (local-set-key [C-down] 'comint-next-input)))

;; Rnw files
;;(local-set-key [(shift return)] 'my-ess-eval))) 
;; cachSweave() compilation with M-n w
;;(define-key noweb-minor-mode-map "\M-nw" 'ess-swv-weave2)


;; bind the list-register function, which has been recently included in emacs
(global-set-key (kbd "C-x r v") 'list-registers)

;; C-c y will show a little pop-up menu with the your kill-menu entries.
(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

;; Now, the M-y key binding will activate browse-kill-ring iff the last command
;; was not a 'yank'.
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;; when column marker is loaded
(global-set-key [?\C-c ?m] 'column-marker-1)


;; magit
(global-set-key (kbd "C-x C-z") 'magit-status)


;; dired
(define-key dired-mode-map "(" 'dired-details-hide)
(define-key dired-mode-map ")" 'dired-details-show)

;; CUA rectangle selection (from the .el file)
;; To start a rectangle, use [C-return] and extend it using the normal
;; movement keys (up, down, left, right, home, end, C-home,
;; C-end). Once the rectangle has the desired size, you can cut or
;; copy it using C-w and M-w, and you can
;; subsequently insert it - as a rectangle - using C-y.  So
;; the only new command you need to know to work with cua-mode
;; rectangles is C-return!
;;
;; Normally, when you paste a rectangle using C-v (C-y), each line of
;; the rectangle is inserted into the existing lines in the buffer.
;; If overwrite-mode is active when you paste a rectangle, it is
;; inserted as normal (multi-line) text.
;;
;; And there's more: If you want to extend or reduce the size of the
;; rectangle in one of the other corners of the rectangle, just use
;; [return] to move the cursor to the "next" corner.  Or you can use
;; the [M-up], [M-down], [M-left], and [M-right] keys to move the
;; entire rectangle overlay (but not the contents) in the given
;; direction.
;;
;; [C-return] cancels the rectangle
;; [C-space] activates the region bounded by the rectangle

;; cua-mode's rectangle support also includes all the normal rectangle
;; functions with easy access:
;;
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge
;;       of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-i] increases the first number found on each line of the rectangle
;;       by the amount given by the numeric prefix argument (default 1)
;;       It recognizes 0x... as hexadecimal numbers
;; [M-k] kills the rectangle as normal multi-line text (for paste)
;; [M-l] downcases the rectangle
;; [M-m] copies the rectangle as normal multi-line text (for paste)
;; [M-n] fills each line of the rectangle with increasing numbers using
;;       a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the
;;       right of the rectangle and filling the rectangle with blanks.
;; [M-p] toggles virtual straight rectangle edges
;; [M-P] inserts tabs and spaces (padding) to make real straight edges
;; [M-q] performs text filling on the rectangle
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)
;; [C-?] Shows a brief list of the above commands.

;; [M-C-up] and [M-C-down] scrolls the lines INSIDE the rectangle up
;; and down; lines scrolled outside the top or bottom of the rectangle
;; are lost, but can be recovered using [C-z].


;==============================
(provide 'kit-bindings)
