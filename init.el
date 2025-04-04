;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Code:
(setq load-prefer-newer t)
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;;; Native compilation and Byte compilation

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-deferred-compilation t
          native-comp-jit-compilation t
          package-native-compile t)
  ;; Ask the user whether to terminate asynchronous compilations on exit.
  ;; This prevents native compilation from leaving temporary files in /tmp.
  (setq native-comp-async-query-on-exit t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(setq native-comp-warning-on-missing-source 0
      native-comp-async-report-warnings-errors 0
      native-comp-verbose  0)

(setq jka-compr-verbose 0)
(setq byte-compile-warnings t
      byte-compile-verbose 0)


;;spesifikasi komputer yang menggunakan fitur paling banyak, dan GUI
;; - JAYAPCCONDET komputer desktop di condet
;; - condetxubuntu2 laptop linux di condet
;; - mesinvm desktop di Prince Center"
;; - GEDE-WIJAYA laptop di Prince Center. Windows
(defvar jaya-emacs/komputer-full
  (or (equal (system-name) "JAYAPCCONDET")
      (equal (system-name) "mesinvm")
      (equal (system-name) "GEDE-WIJAYA")
      (equal (system-name) "condetxubuntu2")))



;; Process performance tuning
;; Increase how much is read from processes in a single chunk
;;(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)
(setq custom-file null-device)
(require 'init-core)
;;(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;;(advice-add #'package-initialize :after #'update-load-path)
;;(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;;(update-load-path)
(require 'package)
(if (string= system-type "windows-nt")
    (setq package-check-signature nil)
  (progn
    (setq gnutls-verify-error t)  ; Prompts user if there are certificate issues
    (setq tls-checktrust t)  ; Ensure SSL/TLS connections undergo trust verification
    (setq gnutls-min-prime-bits 3072)  ; Stronger GnuTLS encryption
    ))

;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))
;; Initialize the packages, avoiding a re-initialization.

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Make sure `use-package' is available.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))
(use-package diminish :ensure t :after use-package) ;; if you use :diminish
(use-package bind-key :ensure t :after use-package) ;; if you use any :bind variant
(require 'init-core)
(require 'init-default)
(require 'init-ui)
(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
