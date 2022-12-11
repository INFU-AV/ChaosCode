;; -*- lexical-binding: t; -*-
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.1)

(setq package-native-compile t)
(setq-default native-comp-speed 3)
;;    native-comp-deferred-compilation nil)

(setq read-process-output-max (* 1024 1024))
(setq process-adaptive-read-buffering nil)

(setenv "LC_ALL" "en_US.UTF-8")
;; (setenv "LANG" "en_US.UTF-8")
(setenv "LANG" "en_GB.UTF-8")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(define-key menu-bar-tools-menu [games] nil)   ; Remove games menu
(setq inhibit-startup-message t)
(menu-bar-mode -1)          ; Disable the menu bar

(modify-all-frames-parameters '((width . 80)
                            (height . 50)
                            (left . 0)
                            (right . 0)
                            (internal-border-width . 1)))
(setq-default frame-title-format '("%b - INFUmacs"))
(setq frame-inhibit-implied-resize t)
(setq file-name-handler-alist nil)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; (defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; GUI transparency
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 40))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 40)))

    ;; (setq idle-update-delay 1.0)  

;; (setq package-enable-at-startup nil)
;; (blink-cursor-mode nil)
