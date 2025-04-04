;;; init-ui.el --- User Interface -*- lexical-binding: t -*-
;;; Code:
;; important for golden-ratio to better work
(setq window-combination-resize t)

;;TODO
;; conflicts with "show-smartparens-mode". see the spacemacs-editing layer
(show-paren-mode -1)

;; no blink
(blink-cursor-mode 0)
;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)
;; draw underline lower
(setq x-underline-at-descent-line t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; make `next-buffer', `other-buffer', etc. ignore useless buffers (see
;; `wijaya-emacs/useless-buffer-p')
(let ((buf-pred-entry (assq 'buffer-predicate default-frame-alist)))
  (if buf-pred-entry
      ;; `buffer-predicate' entry exists, modify it
      (setcdr buf-pred-entry #'wijaya-emacs/useful-buffer-p)
    ;; `buffer-predicate' entry doesn't exist, create it
    (push '(buffer-predicate . wijaya-emacs/useful-buffer-p) default-frame-alist)))
(provide 'init-ui)
;;; init-ui.el ends here
