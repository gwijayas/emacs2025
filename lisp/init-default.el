;;; init-default.el --- default -*- lexical-binding: t -*-
;;; Code:
(tool-bar-mode -1)
(scroll-bar-mode -1)
(unless (display-graphic-p)
  menu-bar-mode -1)

(column-number-mode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)
(when (and (not (daemonp)) (not noninteractive))
  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  ;; Shave seconds off startup time by starting the scratch buffer in
  ;; `fundamental-mode'
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))
;; Allow for shorter responses: "y" for yes and "n" for no.
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; ---------------------------------------------------------------------------
;; Mouse
;; ---------------------------------------------------------------------------

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

(when (boundp 'mouse-wheel-scroll-amount)
  ;; scroll two line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(2)
        ;; don't accelerate scrolling
        mouse-wheel-progressive-speed nil))

;; ---------------------------------------------------------------------------
;; Edit
;; ---------------------------------------------------------------------------

;; bump of the undo limits to avoid issues with premature
;; Emacs GC which truncates the undo history very aggressively
(setq-default
 undo-limit 80000000
 undo-strong-limit 120000000
 undo-outer-limit 360000000)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Text
(setq longlines-show-hard-newlines t)

;; Use system trash for file deletion.
;; This should work on Windows and Linux distros.

(setq delete-by-moving-to-trash t)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 120)

;;TODO
;; persistent abbreviation file
;;(setq abbrev-file-name (concat spacemacs-cache-directory "abbrev_defs"))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;;TODO
;; Prompt to open file literally if large file.
;;(add-hook 'find-file-hook 'spacemacs/check-large-file)

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;;; Backup files

;; Avoid backups or lockfiles to prevent creating world-readable copies of files
(setq create-lockfiles nil)
;;(setq make-backup-files nil)
(setq make-backup-files t)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)  ; Backup by copying rather renaming
(setq delete-old-versions t)  ; Delete excess backup versions silently
(setq version-control t)  ; Use version numbers for backup files
(setq kept-new-versions 5)
(setq kept-old-versions 5)
;;; VC

(setq vc-git-print-log-follow t)
;;(setq vc-make-backup-files nil)  ; Do not backup version controlled files
(setq vc-make-backup-files t)


;;Auto Save
;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
;;(setq auto-save-default nil)
(setq auto-save-default t)
(setq auto-save-no-message t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)
;; Do not auto-disable auto-save after deleting large chunks of
;; text.
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))  ; Resolve issue #29

;;; recentf

;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))

;; Update recentf-exclude
(setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))

;;; saveplace

;; Enables Emacs to remember the last location within a file upon reopening.
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)





(provide 'init-default)
;;; init-default.el ends here
