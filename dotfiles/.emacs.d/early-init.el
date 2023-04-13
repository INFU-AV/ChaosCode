;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold 100000000)

;; Every file opened and loaded by Emacs will run through this list
;; to check for a proper handler for the file,
;; but during startup, it won’t need any of them.
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq site-run-file nil)

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

;; Disable extra interface before it gets initialized
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(define-key menu-bar-tools-menu [games] nil)   ; Remove games menu

(modify-all-frames-parameters
        '((width . 80)
         (height . 35)
         (left . 0)
         (right . 0)
         (internal-border-width . 1)))

(setq frame-inhibit-implied-resize t)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)

;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (convert-standard-filenme
;;       (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; y/n >>>>> yes/no
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; GUI transparency
(set-frame-parameter (selected-frame) 'alpha '(88 . 70))
(add-to-list 'default-frame-alist '(alpha . (95 . 80)))
(setq frame-background-mode 'dark)

;; (setq package-enable-at-startup nil)
;; (blink-cursor-mode nil)

(setq-default frame-title-format '("%b - INFUmacs"))
(setq inhibit-startup-message t)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)