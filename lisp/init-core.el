;;; init-core.el --- Fungsi dan variabel tambahan -*- lexical-binding: t; -*
;;; Code:

(defconst wijaya-emacs/cache-directory
  (concat user-emacs-directory ".cache/")
  "Storage area for persistent files.")

;; ~/.emacs.d/.cache/auto-save
(defconst wijaya-emacs/auto-save-directory
  (concat wijaya-emacs/cache-directory "auto-save/")
  "Auto-save directory.")

;;;; Setup cache directories

(make-directory wijaya-emacs/cache-directory 'parents)

(defun font-exists-p (font)
  (if (null (x-list-fonts font))
      nil t))

(defun wijaya-emacs/activate-recentf ()
  "recentf is an Emacs package that maintains a list of recently
accessed files, making it easier to reopen files you have worked on
recently."
  (add-hook 'after-init-hook #'(lambda()
                                 (let ((inhibit-message t))
                                   (recentf-mode 1))))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(defun wijaya-emacs/activate-revert-mode ()
  "Auto-revert in Emacs is a feature that automatically updates the
contents of a buffer to reflect changes made to the underlying file
on disk."
  (add-hook 'after-init-hook #'global-auto-revert-mode))

(defun wijaya-emacs/activate-save-place-mode ()
  "save-place-mode enables Emacs to remember the last location within a file
upon reopening. This feature is particularly beneficial for resuming work at
the precise point where you previously left off."
  (add-hook 'after-init-hook #'save-place-mode))

(defun wijaya-emacs-activate-savehist-mode ()
  "savehist is an Emacs feature that preserves the minibuffer history between
sessions. It saves the history of inputs in the minibuffer, such as commands,
search strings, and other prompts, to a file. This allows users to retain
their minibuffer history across Emacs restarts"
  (progn
    (setq history-length 300)
    (setq savehist-save-minibuffer-history t)  ;; Default
    (setq savehist-additional-variables
          '(kill-ring                        ; clipboard
            register-alist                   ; macros
            mark-ring global-mark-ring       ; marks
            search-ring regexp-search-ring)) ; searches

    (add-hook 'after-init-hook #'savehist-mode)
    )
  )

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar wijaya-emacs/useless-buffers-regexp '()
  "Regexp used to determine if a buffer is not useful.

Such useless buffers are skipped by `previous-buffer',
`next-buffer', and, optionally, by `wijaya-emacs/alternate-buffer'
(see `wijaya-emacs/useful-buffers-restrict-spc-tab').")


(defvar wijaya-emacs/useful-buffers-regexp '()
  "Regexp used to define buffers that are useful despite matching
`wijaya-emacs/useless-buffers-regexp'.")


(defun wijaya-emacs/useful-buffer-p (buffer)
  "Return non-nil if BUFFER should be offered when switching buffers."
  (let ((buf-name (buffer-name buffer)))
    (or (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'comint-mode)
        (cl-loop for useful-regexp in wijaya-emacs/useful-buffers-regexp
                 thereis (string-match-p useful-regexp buf-name))
        (cl-loop for useless-regexp in wijaya-emacs/useless-buffers-regexp
                 never (string-match-p useless-regexp buf-name)))))

(defun wijaya-emacs/useless-buffer-p (buffer)
  "Return non-nil if BUFFER should be ignored when switching buffers."
  (not (wijaya-emacs/useful-buffer-p buffer)))


(provide 'init-core)
;;; init-core.el ends here
