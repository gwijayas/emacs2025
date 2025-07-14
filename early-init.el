;;; early-init.el --- -*- no-byte-compile: t -*-
;;; Daftar referensi:
;;; - https://github.com/redguardtoo/emacs.d
;;; - https://github.com/jamescherti/minimal-emacs.d
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

(when (or (featurep 'esup-child)
          (daemonp)
          noninteractive)
  (setq package-enable-at-startup nil))

;; @see https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/
;; 10% speed up of startup for my configuration
(when jaya-emacs/komputer-full
  (progn
    (setq gc-cons-percentage 0.6)
    (setq gc-cons-threshold most-positive-fixnum)))

(setq inhibit-startup-message t)
;; no menu bar, toolbar, scroll bar
;; (setq default-frame-alist
;;       '((menu-bar-lines . 0)
;;         (tool-bar-lines . 0)
;;         (horizontal-scroll-bars)
;;         (vertical-scroll-bars)))

(provide 'early-init)
;;; early-init ends here
