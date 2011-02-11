;; ------------------------------------- menus
;; TODO read http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; TODO imenu gives a list of all functions in a file, and lets you jump to them. idomenu (http://emacswiki.org/emacs/idomenu.el) gives you ido functionality for imenu. 

;; one could also use iCicles or Anything, instead of IDO
;; TODO read http://www.emacswiki.org/emacs/InteractivelyDoThings

;; find-file-at-point when it makes sense
(setq ffap-machine-p-known 'accept) ; no pinging
(setq ffap-url-regexp nil) ; disable URL features in ffap
(setq ffap-ftp-regexp nil) ; disable FTP features in ffap
(define-key global-map (kbd "C-x C-f") 'find-file-at-point)

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq ido-create-new-buffer 'always)
;; ;; Jump to a definition in the current file.
;; (global-set-key "\C-x\C-i" 'ido-imenu)
;; ;; File finding
;; (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
;; (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; (global-set-key (kbd "C-x C-p") 'find-file-at-point)
;; (global-set-key (kbd "C-c y") 'bury-buffer)
;; (global-set-key (kbd "C-c r") 'revert-buffer)
;; (global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)


(require 'ibuffer)
(global-set-key "\C-x\C-b" 'ibuffer)

;; use iswitchb-mode for C-x b
(iswitchb-mode)


;; ------------------------------------- mark and transient mark mode
;; explicitly set the mark. bind it to C-`, an unused key.

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

;; jump to the mark 
;; replaces the binding on M-`  (command that opens up a terminal-friendly menu bar in the minibuffer)
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

;; when you exchange point and mark, do not activate. 
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; --------------------------------------------- registers and rings

;; bind the list-register function, which has been recently included in emacs
(global-set-key (kbd "C-x r v") 'list-registers)

;; Now, the M-y key binding will activate browse-kill-ring iff the last command
;; was not a 'yank'.
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;; C-c y will show a little pop-up menu with the your kill-menu entries.
(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

;; (setq-default kill-ring-max 60)




;; ----------------------------------------------- windows

;; This is to switch the split from horizontal to vertical
;; Idea and starter code from Benjamin Rutt (rutt.4+news@osu.edu) on comp.emacs
(defun split-window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
	(buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))


;; complement of above created by rgb 11/2004
(defun split-window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
	(buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;; from emacs wiki
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))


;; set the keybindings, 'C-x 4' is where most of the gnu emacs binding for new
;; windows exist
(global-set-key (kbd "C-x 4 t") 'transpose-buffers)
(global-set-key (kbd "C-x 4 h") 'split-window-vertical-to-horizontal)
(global-set-key (kbd "C-x 4 v") 'split-window-horizontal-to-vertical)




;; ===========================================================================
(provide 'kit-navigation)
