;;; early-init.el --- -*- no-byte-compile: t -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; However, Emacs 27 also loads the "early init" file (this file)
;; before it initializes the package manager
;; Earlier Emacs versions do not load the early init file and do not initialize
;; the package manager before loading the init file, so this file is neither
;; needed nor loaded on those versions.
(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)
;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;;; early-init ends here
