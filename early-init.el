;;; early-init.el --- -*- no-byte-compile: t -*-
;;; Daftar referensi:
;;; - https://github.com/redguardtoo/emacs.d
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

(defvar my-computer-has-smaller-memory-p nil
  "Installing&Compiling many packages could cost too much memory.")


;; hardcode saja mesin-mesin besar

;; @see https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/
;; 10% speed up of startup for my configuration
(unless my-computer-has-smaller-memory-p
  (setq gc-cons-percentage 0.6)
  (setq gc-cons-threshold most-positive-fixnum))

;;; early-init ends here
