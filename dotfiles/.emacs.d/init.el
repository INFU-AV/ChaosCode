;;; init.el --- -*- lexical-binding: t -*-
;; -*- lexical-binding: t; -*-

(setq user-login-name "INFU")

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
; - - FORMATTING WARNING - -
;I format packages in way that
; long lines are least idented 
; for better readibility on
; phones & thin small screens 
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;    .dotfiles to learn from:
; https://github.com/daviwil/emacs-from-scratch
; https://github.com/daviwil/dotfiles/blob/master/Emacs.org
; https://github.com/SqrtMinusOne/dotfiles/blob/master/Emacs.org
; https://taingram.org/init.html
; https://sachachua.com/dotemacs/index.html
; https://github.com/DiamondBond/emacs/blob/master/config.org
; https://github.com/bbatsov/prelude
;
; ...and countless articles online I cannot count
;
; WIN  ;; for Windows:
; https://www.emacswiki.org/emacs/CategoryWThirtyTwo
; https://www.gnu.org/software/emacs/manual/html_node/emacs/Microsoft-Windows.html
; https://www.gnu.org/software/emacs/manual/html_node/emacs/MS_002dWindows-Registry.html

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
    ;;  STRUCTURE:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Crucial:
;; ----- Navigation/KeyCombos:
;; ----- Dired-stuff:
;; ----- Markdown-stuff:
;; ----- Functionality:
;; ----- Beauty/visibility:
;; ----- Termux:
;; ----- MS-Windows:
;; ----- Closing:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

(defun infu/display-startup-time ()
 "Personalized startup timer.
The Yak-Shaving necessity!"
    ( message
">->->[LOADING TIME >>> %s]<-<-<
 >->-> GARBAGE COLLECTED -> (%.2d) <-<-<"
 (format "%.3f sec!"
    (float-time
 (time-subtract after-init-time before-init-time)))
    gcs-done))
; Let's start this timer now! ;
(add-hook 'emacs-startup-hook #'infu/display-startup-time)

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Crucial:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

    ; Initialize package loader ;
    	(require 'package)

    ; Initialize package sources ;
(setq package-archives
    '(("melpa"  . "https://melpa.org/packages/")
      ("elpa"   . "https://elpa.gnu.org/packages/")
      ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

    ; Install "use-package" if missing ;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
(package-install 'use-package))

    ; organized way to configure packages ;
    	(require 'use-package)

;; Emacs load configurations
;;  from the top to the bottom.
;; If package or setting fails,
;;  loading gets interrupted!

;; DEBUG PROFILER:
; https://blog.d46.us/advanced-emacs-startup/

    ; ===== DEBUGGING: ===== ;	
; (Setq debug-on-error t)
;;  ; ..or  'M-x "toggle-debug-on-error"'
; M-x "redraw-display"
;;  ; ..should get rid of visual glitches
; M-x "memory-report" ; to see summary of
;;  ;    for bootup-time analysis:
; (setq use-package-compute-statistics 1)
;;  ; then use M-x "use-package-report"
;;  ; to see stats from function above
;; (setq use-package-verbose t) ; see what packages get loaded in detail
;; (setq use-package-minimum-reported-time 0.0001)
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

;; Emacs has neat way of setting up
;;  kinda easy customisations,
;;  but they clutter your init.el heavily!
;; We set separate file for it:
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Also, there's difference between
;;  loading customisations early in init.el
;;  or right by the very end!
;; Stick to one that works for you


;; If custom.el doesn't exist, packages probably too!
;; my sneaky way of deploying from scratch:
(unless (file-exists-p custom-file)
 (unless package-archive-contents
 (package-refresh-contents))
 (setq use-package-always-ensure t))
    ; Write to it if it does not exist
(unless (file-exists-p custom-file)
 (write-region "" nil custom-file))

    ; Finally, load custom file!
    ; Hide success message, not errors
(load custom-file nil t)

      	(use-package no-littering
        ;makes sure other packages won't make mess;
    :init
(setq no-littering-etc-directory
 (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(setq auto-save-file-name-transforms
 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


        (use-package files
    :init
;; Save backup files to a dedicated directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t
    delete-old-versions t
    version-control t
    kept-new-versions 4
    kept-old-versions 2
    make-backup-files t)
;; Do not create #lock #files.
(setq create-lockfiles nil)
(setq auto-save-timeout 3)
(setq delete-old-versions t)
(auto-save-visited-mode t))

    	(use-package evil
;vim-style modal keybinding & workflow;
 ;; Makes Emacs on Touch-Keyboard
 ;; actually pleasure to use!
    ;; Manuals:
;; https://evil.readthedocs.io
;; https://github.com/noctuid/evil-guide
    :init
(setq evil-move-beyond-eol t
 evil-undo-system 'undo-fu
 evil-want-C-i-jump nil
 evil-esc-delay 0.02
 evil-want-integration t ;; This is optional since it's already set to t by default.
 evil-want-keybinding nil
 evil-auto-indent nil
 evil-move-cursor-back nil
 evil-kill-on-visual-paste nil)
    :config
(setq  evil-search-module 'isearch)
;; (setq  evil-search-module 'evil-search)
;; difference?
;; apparently when you want to operate on search:
;; source: https://medium.com/@lynzt/emacs-evil-evil-search-mode-and-the-cgn-command-839c633ba7f3
(setq  evil-flash-delay 2)
(setq evil-ex-hl-update-delay 0.5)
(evil-mode))

    	(use-package evil-collection
	;more cool bindings for evil;
    :defer 2
    :init
; for recording information, don't need it
(setq annalist-record nil)
    :config
(evil-collection-init `(
    apropos
    (buff-menu "buff-menu")
    compile
    dired
    elfeed
    elisp-mode
    elisp-refs
    ,@(when (>= emacs-major-version 29) '(emoji))
    finder
    free-keys
    grep
    help
    ibuffer
    imenu
    imenu-list
    (indent "indent")
    markdown-mode
    org
    outline
    (package-menu package)
    pass
    (process-menu simple)
    python
    sh-script
    ,@(when (>= emacs-major-version 28) '(shortdoc))
    simple
    ,@(when (>= emacs-major-version 27) '(tab-bar))
    ;; tabulated-list
    (term term ansi-term multi-term)
    ,@(when (>= emacs-major-version 27) '(thread))
    which-key
)))

        (use-package undo-fu
    ;undo functionality for Evil
    :config (setq undo-limit 3355443200) ;; 32mb.
;; (setq undo-strong-limit 100663296) ;; 96mb.
;; (setq undo-outer-limit 1006632960) ;; 960mb.
)

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Navigation/KeyCombos:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

(xterm-mouse-mode 1)

        (use-package crux
    :defer t)
 
(use-package which-key
    :defer 3
    :custom
(setq which-key-max-display-columns 8
;; (setq which-key-popup-type 'minibuffer)
  which-key-popup-type 'side-window
  which-key-side-window-location 'top
  which-key-separator " ‧ "
  which-key-idle-delay 1) 
    :config
(which-key-mode))

    ;; my custom keymap: INFU-map ;;
(define-prefix-command 'infu-map)
;Those are couple of keybindings I made to fit;
;my workflow using Termux & mobile keyboards ;
;GOALS: Functionality that's close & familiar;
    ;; 2 ways to summon it:
(define-key evil-motion-state-map (kbd ",") 'infu-map)

(global-set-key  (kbd "\e,") `infu-map) ; "ESC-," in case one above is occupied
    ;; init.el yak shaving set:
(define-key infu-map (kbd "e") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))
(define-key infu-map (kbd "C-e") 'emacs-init-time)
(define-key infu-map (kbd "M-e") 'crux-find-shell-init-file)
(define-key infu-map (kbd "E") 'eval-buffer)
(define-key infu-map (kbd "p") 'list-packages)
;; (define-key infu-map (kbd "SPC") 'eval-region)

(define-key infu-map (kbd ".") 'describe-char)

    ;; Ease of life functions
(define-key infu-map (kbd ".") 'evil-window-prev)
(define-key infu-map (kbd ",") 'evil-window-next)
(define-key infu-map (kbd "y") 'yank-from-kill-ring) 
(define-key infu-map (kbd "(") 'check-parens) 

; https://puntoblogspot.blogspot.com/2018/11/evilmacs-macros.html
(define-key infu-map (kbd "Z") 'kmacro-start-macro)
(define-key infu-map (kbd "z") 'kmacro-end-or-call-macro)

(define-key infu-map (kbd "f") 'find-file)
(define-key infu-map (kbd "F") 'find-file-other-tab)

(define-key infu-map (kbd "b") 'switch-to-buffer)
(define-key infu-map (kbd "B") 'switch-to-buffer-other-tab)

(define-key infu-map (kbd "g") 'crux-duplicate-and-comment-current-line-or-region)

(define-key infu-map (kbd "k") 'grt/kill-current-buffer) ;(B;)
(define-key infu-map (kbd "K") 'kill-buffer-and-window)

(define-key infu-map (kbd "C-k") 'tab-close)

(define-key infu-map (kbd "d") 'dired)
 ; Rewrite this one as relative path:
(define-key infu-map (kbd "a") 'list-abbrevs)
(define-key infu-map (kbd "A") (lambda() (interactive)(find-file "$HOME/.emacs.d/my-abbrev.el"))) 
(define-key infu-map (kbd "t") 'tab-bar-new-tab)
(define-key infu-map (kbd "M-t") (lambda() (interactive)(find-file "$HOME/xinfu/todo.md")))
(define-key infu-map (kbd "TAB") 'hs-minor-mode)
(define-key infu-map (kbd "C-m") 'bookmark-bmenu-list)
(define-key infu-map (kbd "m") 'bookmark-jump)
(define-key infu-map (kbd "M") 'bookmark-set)

(define-key infu-map (kbd "j") 'term)

(define-key infu-map (kbd "!") 'delete-window)
(define-key infu-map (kbd "1") 'delete-other-windows)
(define-key infu-map (kbd "2") 'split-and-follow-horizontally) ;(B);
(define-key infu-map (kbd "M-2") '2C-split)
(define-key infu-map (kbd "3") 'split-and-follow-vertically) ;(B);
(define-key infu-map (kbd "M-3") '2C-two-columns)

(define-key infu-map (kbd "4") 'follow-delete-other-windows-and-split)

(define-key infu-map (kbd "<next>") 'scroll-other-window)
(define-key infu-map (kbd "<prior>") 'scroll-other-window-down)
;; since normal-mode "m" is marking things, we go back to them with	
( define-key evil-normal-state-map (kbd "M") 'evil-goto-mark)
;; ( define-key evil-normal-state-map (kbd "l") "")
(fset 'shfunc
   (kmacro-lambda-form [?O ?f ?u ?n ?c ?\C-? ?\C-? ?\C-? ?u ?n ?1 ?  ?\( ?\C-\[ ?O ?C ?  ?\{ ?\C-\[ ?\[ ?3 ?~ ?\C-\[ ?O ?B ?\C-\[ ?O ?H ?\C-\[ ?O ?B ?\} ?\C-m escape] 0 "%d"))

;; --- EVIL INFU
(define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
(define-key evil-visual-state-map (kbd "SPC") 'eval-region)
(define-key evil-normal-state-map (kbd "C-e") 'eval-last-sexp)

(define-key evil-visual-state-map (kbd "<") '+evil/shift-left)
(define-key evil-visual-state-map (kbd ">") '+evil/shift-right)

(define-key evil-visual-state-map (kbd "gp") '+evil/alt-paste)

(define-key evil-motion-state-map (kbd ",") 'infu-map)
(define-key evil-visual-state-map (kbd "g c") 'comment-dwim)
(define-key evil-normal-state-map (kbd "g c") 'comment-line)
(define-key evil-normal-state-map (kbd "g C-c") 'crux-duplicate-and-comment-current-line-or-region)

(define-key evil-motion-state-map (kbd "<next>") 'evil-forward-paragraph)
(define-key evil-motion-state-map (kbd "<prior>") 'evil-backward-paragraph)

(define-key evil-insert-state-map (kbd "C-O") 'evil-open-below)
(define-key evil-insert-state-map (kbd "C-o" ) 'evil-open-above)
;; soon adding hyper key
(global-set-key (kbd "H-f" ) 'ffap)


	(use-package hydra
	;more key combo/menus;
    :config
    ;; (setq hydra-lv t)
    ;; (setq lv-use-separator t)
  (defhydra hydra-window ( :columns 4 :hint nil )
    "Resizing"
   ("<down>"  evil-window-decrease-height "↓")
   ("<left>"  evil-window-decrease-width "←")
   ("<right>" evil-window-increase-width "→")
   ("<up>"    evil-window-increase-height "↑")
   (":"       delete-window "" :color blue )
   ("1"       delete-other-windows "☠" :color blue )
   ("2"       split-window-below "✂" :color blue )
   ("3"       split-window-right "✄" :color blue )
   ("8"       balance-windows "⚖" :color blue ))
(define-key infu-map (kbd "W") 'hydra-window/body) ; capital W
  (defhydra hydra-tab-bar (:color amaranth)
   "Tab Bar Operations:"
   ("t" tab-new "Just new" :column "New Tab")
   ("d" dired-other-tab "Dired")
   ("f" find-file-other-tab "Find file")
   ("m" tab-move "Move it" :column "This Tab")
   ("r" tab-rename "Rename it")
   ("0" tab-close "Close it")
   ("<right>" tab-next "Tab ->" :column "Navigation")
   ("<left>" tab-previous "<- Tab")
   ("q" nil "Exit" :exit t))
(define-key infu-map (kbd "w") 'hydra-tab-bar/body) ; small w
)

        (use-package abbrev
    :defer 2
    ; abbrev-mode xah style ; http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
    :config
(setq abbrev-file-name "~/.emacs.d/my-abbrev.el")
; command to start abbrev-mode is inside abbrev file
(setq abbrev-suggest 't)
(setq abbrev-suggest-hint-threshold 1)
)


;; ; Turn off unwanted modes
;; (dolist (this-minor-mode
;;          '(csv-field-index-mode
;;            diff-auto-refine-mode
;;            file-name-shadow-mode
;;           auto-encryption-mode
;;            global-magit-file-mode
;;            treemacs-filewatch-mode
;;            treemacs-follow-mode
;;            treemacs-git-mode
;;            Shell-Dirtrack
;;            prettify-symbols-mode
;;            global-prettify-symbols-mode
;;            treemacs-fringe-indicator-mode))
;;   (when (fboundp this-minor-mode)
;;     (funcall this-minor-mode 0)))

        (use-package helpful
    :defer 2
    :bind
(("C-h k" . helpful-key)
 ("C-h h" . helpful-at-point)
 ("C-h v" . helpful-variable)
 ("C-h F" . helpful-function)
 ("C-h C" . helpful-command)))

  ;; disabled as Im trying vertico ;;
;;         (use-package ido
;; ;; (setf (nth 2 ido-decorations) "\n")
;;     :init (setq ido-separator "\n")
;; ;; show any name that has the chars you typed
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere 1)
;;     :config (ido-mode 1))

;; 	(use-package icomplete
;;     ;; show choices vertically
;; 	;; :ensure nil
;;     :defer 1
;;     :init
;; (setq icomplete-separator "\n")
;; (setq icomplete-hide-common-prefix nil)
;; (setq icomplete-in-buffer t)
;;     :config
;; (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
;; (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)
;; (define-key icomplete-minibuffer-map (kbd "<backtab>") 'icomplete-force-complete)
;; (define-key icomplete-minibuffer-map (kbd "M-<RET>") 'icomplete-force-complete-and-exit)
;; (icomplete-mode 1)
;; (setq completion-cycle-threshold 4)
;; )

        (use-package marginalia
    :init (marginalia-mode t)
  :config
(setq marginalia--ellipsis "…"    ; Nicer ellipsis
      marginalia-align 'right     ; right alignment
      marginalia-align-offset -1) ; one space on the right
  :bind ("C-c m" . marginalia-mode))


        (use-package vertico
    :custom (vertico-cycle t)
    :init
(vertico-mode)
(define-key vertico-map (kbd "C-S-n") 'vertico-next)
(define-key vertico-map (kbd "C-S-p") 'vertico-previous)
(define-key vertico-map "?" #'minibuffer-completion-help)
(define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
(define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)

(setq crm-separator ",")
;; Add prompt indicator to `completing-read-multiple'.
(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq vertico-resize nil         ; How to resize the Vertico minibuffer window.
       vertico-count 7           ; Maximal number of candidates to show.
       vertico-count-format nil) ; No prefix with number of entries
(setq vertico-grid-separator
    #("  |  " 1 2 (display (space :width (1))
                            face (:background "#ECEFF1")))
    vertico-group-format
    (concat #(" " 0 1 (face vertico-group-title))
            #(" " 0 1 (face vertico-group-separator))
            #(" %s " 0 4 (face vertico-group-title))
            #(" " 0 1 (face vertico-group-separator
                        display (space :align-to (- left (-1 . left-margin) (- +1)))))))
(set-face-attribute 'vertico-group-separator nil
                    :strike-through t)
(set-face-attribute 'vertico-current nil
                    :inherit '(Infu-Red))
(set-face-attribute 'completions-first-difference nil
                    :inherit '(Infu-Yellow))
  ;; minibuffer tweaks
(defun my/vertico--resize-window (height)
"Resize active minibuffer window to HEIGHT."
    (setq-local truncate-lines t
                resize-mini-windows 'grow-only
                max-mini-window-height 1.0)
(unless (frame-root-window-p (active-minibuffer-window))
    (unless vertico-resize
    (setq height (max height vertico-count)))
    (let* ((window-resize-pixelwise t)
            (dp (- (max (cdr (window-text-pixel-size))
                        (* (default-line-height) (1+ height)))
                (window-pixel-height))))
    (when (or (and (> dp 0) (/= height 0))
                (and (< dp 0) (eq vertico-resize t)))
        (window-resize nil dp nil nil 'pixelwise)))))
(advice-add #'vertico--resize-window :override #'my/vertico--resize-window)
)

        (use-package savehist
    :config (savehist-mode 1))

	(use-package auto-complete
    :defer 1
    :config
(ac-config-default)
(auto-complete-mode 1))

;; 	(use-package orderless
;;     :defer 1
;;     :init
;; (setq completion-styles '(orderless basic)
;; completion-category-defaults nil
;; completion-category-overrides '((file (styles basic partial-completion initials)))))
        (use-package orderless
    :defer 1
    :init
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides
        '((file (styles basic partial-completion)))))

    ;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
    ;; to not overload my init.el with
    ;; tons blocks of code, i split it here:
(load "~/.emacs.d/bonus.el" nil t)
    ;; Code refferencing stuff in bonus.el
    ;; will be marked this way: ;(B);
    ;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
    ;;
    ;;

        (use-package infu-bionic-face ;(B);
    :commands
(infu-bionic-reading-buffer - infu-bionic-reading-region))
 
; M-x ^
    ;; xah-dired-sort ;(B);
(evil-define-key 'normal dired-mode-map (kbd "s") 'xah-dired-sort)
    ;;
    ;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

        (use-package winner
    :defer 1
	      ; undo-window changes ;
	;; :diminish windmove-mode
    :config (winner-mode t)
    :bind
("C-c <left>"  . winner-undo)
("C-c <right>" . winner-redo))

        (use-package delsel
	      ; replace selection ;
    :config (delete-selection-mode 1))

        (use-package evil-surround
    ; https://github.com/howardabrams/dot-files/blob/master/emacs-evil.org
    :config (global-evil-surround-mode 1))

	(use-package move-text
	; move lines up/down ;
    ;; :config (move-text-default-bindings))
    :bind
("ESC <up>" . move-text-up)
("M-<up>" . move-text-up)
("ESC <down>" . move-text-down)
("M-<down>" . move-text-down))

; Clean unsaved buffers after 600 seconds
(run-with-idle-timer 600 t (lambda ()
                               (my-clean-frames-and-buffers)))
(defun my-clean-frames-and-buffers ()
  "Kills all unmodified buffers and closes all but the selected frame."
  (interactive)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (and (buffer-live-p buffer)
           (not (buffer-modified-p buffer))
           (kill-buffer buffer))))
  (delete-other-frames))

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Networking:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

        (use-package org-ehtml
    :defer 5
    :config
(defun ehtml-start () 
(ws-start org-ehtml-handler 8888))
    :commands (ehtml-start))

        (use-package org-protocol
    :defer 5
    :init
    :config
)

 ;; emacs multiplayer package ;;
        (use-package crdt  :disabled)

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Dired-stuff:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

	(use-package dired
    :defer 2
    ;; :ensure nil
    ;; :hook (dired-mode . 
    :config 
(evil-define-key 'normal 'dired-mode-map
(kbd "C-n") 'dired-create-empty-file
(kbd "C-N") 'dired-create-directory)
(setq dired-kill-when-opening-new-dired-buffer t) ; NEW in Emacs 28
(setq dired-dwim-target t) ; suggest file-path of another open Dired buffer if there is one
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
    :custom
((dired-listing-switches "-agho --group-directories-first")))

	(use-package diredfl
    :defer 2
    :after dired
    :hook (dired-mode . diredfl-mode))

        (use-package autorevert
    :config
(setq global-auto-revert-non-file-buffers t) ; autoupdate Dired
(global-auto-revert-mode 1)) ; autoupdate files


;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Markdown-stuff:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
    ;; Manuals:
;; https://leanpub.com/markdown-mode
 
	(use-package markdown-mode
    :defer 1
    :mode ("README\\.md\\'" . gfm-mode)
    :config
(setq markdown-command "pandoc"
  markdown-reference-location "immediately"
  markdown-url-compose-char "°"
  markdown-hide-urls t)
(infu/faces))

        (use-package markdown-preview
    :defer t
    :commands (markdown-preview-mode)
)

     	(use-package zk
    :defer t
    :custom
(zk-directory "~/xinfu/notes")
(zk-file-extension "md")
    :config
(zk-setup-auto-link-buttons)
    (defhydra hydra-zk ( :columns 2 :color blue :hint none)
"===== zk  =====
[n]ew,[f]ind,[l]ink in, ([L] for new)
[s]earch,[t]ag search / [i]nsert"
("n" zk-new-note)
("f" zk-find-file)
("l" zk-insert-link)
("b" zk-backlinks)
("s" zk-search)
("i" zk-tag-insert)
("t" zk-tag-search))
   :bind (:map infu-map
("c" . hydra-zk/body)
("C" . zk-index)))

;;       	(use-package zk-index
;;     :after zk
;;     :config 
;;  	    (defvar zk-index-mode-map
;;     (let ((map (make-sparse-keymap)))
;; (define-key map (kbd "n") #'zk-index-next-line)
;; (define-key map (kbd "p") #'zk-index-previous-line)
;; (define-key map (kbd "v") #'zk-index-view-note)
;; (define-key map (kbd "o") #'other-window)
;; (define-key map (kbd "f") #'zk-index-focus)
;; (define-key map (kbd "s") #'zk-index-search)
;; (define-key map (kbd "d") #'zk-index-send-to-desktop)
;; (define-key map (kbd "D") #'zk-index-switch-to-desktop)
;; (define-key map (kbd "c") #'zk-index-current-notes)
;; (define-key map (kbd "i") #'zk-index-refresh)
;; (define-key map (kbd "S") #'zk-index-sort-size)
;; (define-key map (kbd "M") #'zk-index-sort-modified)
;; (define-key map (kbd "C") #'zk-index-sort-created)
;; (define-key map (kbd "RET") #'zk-index-open-note)
;; (define-key map (kbd "q") #'delete-window)
;; (make-composed-keymap map tabulated-list-mode-map))
;;   "Keymap for ZK-Index buffer.")
;; (zk-index-setup-embark)
;;     :custom (zk-index-desktop-directory zk-directory))

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Functionality:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

;; (global-subword-mode 1) 

      	(use-package xclip
	; Android clipboard integration
    :unless (eq system-configuration 'aarch64-unknown-linux-android)
    :config (xclip-mode 1))

; ↓ setting default browser so
; ↓ Emacs asks which one to use per link
; ↓ Android-specific, requires "termux-api"
    (advice-add 'browse-url-default-browser :override
(lambda (url &rest args)
(start-process-shell-command "open-url" nil (concat "am start -a android.intent.action.VIEW -d " url))))
 (global-set-key [mouse-3] #'ffap-at-mouse)


        (use-package electric
	; turn off all indents as I do my own here ;
    :config (electric-indent-mode 0)
(electric-pair-mode 1))

        (use-package saveplace
	; remember cursor position in files ;
    ;; :diminish save-place-mode
    :config (save-place-mode 1))

      	(use-package hideshow)

	(use-package command-log-mode
	;log commands you typed in;  
    :custom (command-log-mode-auto-show t)
(command-log-mode-window-size 30)
    :commands (command-log-mode))

      	(use-package ca65-mode
	    ;NES syntax;  
    :commands (ca65-mode))

      	(use-package free-keys
	 ;look for free keys;
    :commands (free-keys))

        (use-package tab-bar
    :defer 1
    :config 
(define-key evil-normal-state-map (kbd "g C-t") 'tab-bar-new-tab)
(define-key evil-normal-state-map (kbd "g M-t") 'tab-close)
(define-key evil-normal-state-map (kbd "t") 'tab-bar-switch-to-next-tab)
(define-key evil-normal-state-map (kbd "T") 'tab-bar-switch-to-prev-tab)

(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item
    (propertize "-INFU-"
                        'face 'Infu-Red)
            tab-bar-menu-bar '(message "lmao"))))

(setq tab-bar-format
    '(tab-bar-format-menu-bar
      tab-bar-format-history
      tab-bar-format-tabs
      tab-bar-separator
      tab-bar-format-align-right
      tab-bar-format-global))

(tab-bar-mode))

; giving some kind of usefulness to scratch
(setq initial-scratch-message (infu/display-startup-time))
;; (setq initial-scratch-message (current-time-string))

	(use-package tooltip
    ;; :ensure nil
    :config (tooltip-mode 0))

        (use-package elfeed
    :defer 2
    ;; :init
    :config
; https://nullprogram.com/blog/2013/09/04/
; https://lucidmanager.org/productivity/read-rss-feeds-with-emacs-and-elfeed/
; https://blog.dornea.nu/2022/06/29/rss/atom-emacs-and-elfeed/
;
(setq elfeed-feeds 
'(("https://www.littlesounddj.com/lsd/latest/CHANGELOG.rss" chiptu)
  ("https://chipmusic.org/music/rss/feed.xml" chiptune)
; ("YOURLINKHERE" TAGHERE)
))
(setq shr-width 80) ;; Read view narrowing
)

(setq use-dialog-box nil);; don't use dialog boxes to ask questions
(setq use-file-dialog nil);; don't use a file dialog to ask for files

(setq inhibit-startup-echo-area-message "INFU")

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Beauty/visibility:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

; italizing/bolding text stuff
; https://github.com/localauthor/.emacs.d/blob/main/init.el#L228

(defun infu/faces()
(make-face 'Infu-Red)
(set-face-attribute 'Infu-Red nil
    :foreground "#FF4444"
    :background "#000000"
    :weight 'ultra-bold
    :overline "white"
    ;; :slant 'italic
    :underline "white")
(make-face 'Infu-Yellow-bg)
(set-face-attribute 'Infu-Yellow-bg nil
;; :foreground "#000000"
    :background "#333300"
    ;; :foreground "#333300"
    ;; :background "#FFFF00"
    ;; :weight 'ultra-bold
    ;; :slant 'oblique
    :overline "#FFFF00"
    :underline "#FFFF00")
(make-face 'Infu-Yellow)
(set-face-attribute 'Infu-Yellow nil
    :foreground "#FFFF00"
    :background "#333300"
    :weight 'ultra-bold
    ;; :slant 'oblique
    :underline 't)
(make-face 'Infu-Blue)
(set-face-attribute 'Infu-Blue nil
    :foreground "#44aaff"
    :background "#000033"
    :weight 'bold
    :slant 'normal
    :underline 't)
(make-face 'Infu-Green)
(set-face-attribute 'Infu-Green nil
    :foreground "#00ff00"
    :background "#003300"
    :weight 'ultra-bold
    :slant 'normal
    :underline 't)
(make-face 'Infu-Purple)
(set-face-attribute 'Infu-Purple nil
    :foreground "#FF33FF"
    :background "#330033"
    ;; :weight 'bold
    ;; :slant 'reverse-oblique
    ;; :underline 't
)



(copy-face 'Infu-Red 'markdown-header-face-1 )
(copy-face 'Infu-Yellow 'markdown-header-face-2 )
(copy-face 'Infu-Blue 'markdown-header-face-3 )
(copy-face 'Infu-Green 'markdown-header-face-4 )
(copy-face 'Infu-Purple 'markdown-header-face-5 )
(copy-face 'markdown-header-face-3 'ido-subdir )
(copy-face 'markdown-header-face-4 'ido-only-match )
(copy-face 'markdown-header-face-3 'minibuffer-prompt )
(copy-face 'markdown-header-face-4 'tab-bar-tab )
(copy-face 'markdown-header-face-3 'tab-bar )
(copy-face 'markdown-header-face-3 'tab-bar-tab-inactive )
(copy-face 'Infu-Yellow 'region)
(copy-face 'markdown-header-face-4 'mode-line )
(copy-face 'Infu-Yellow 'doom-modeline-highlight)

(copy-face 'markdown-header-face-2 'show-paren-match)
(copy-face 'markdown-header-face-3 'mode-line-inactive)

(set-face-attribute 'default nil
:foreground "#FFFFFF"
:background "gray2")

(copy-face 'markdown-header-face-4 'mode-line-highlight)
)
      	(use-package evil-terminal-cursor-changer
	;first cosmetics I installed, beautify cursors;  
    :custom
(setq etcc-use-color t)
;; (setq etcc-term-type-override 'Dumb)
    :config (setq
evil-insert-state-cursor   '("blue" box)
evil-emacs-state-cursor    '("magenta" hbar)
evil-normal-state-cursor   '("green" box)
evil-visual-state-cursor   '("orange" box)
evil-operator-state-cursor '("red" hbar)
evil-replace-state-cursor  '("magenta" bar)
evil-motion-state-cursor   '("yellow" box))
; newest update seems to kill my colors and shapes, this fixes it:
(defadvice evil-set-cursor (after etcc--evil-set-cursor (arg) activate)
  (unless (display-graphic-p)
    (etcc--evil-set-cursor)))
(defadvice evil-set-cursor-color (after etcc--evil-set-cursor (arg) activate)
    (unless (display-graphic-p)
    (etcc--evil-set-cursor-color arg)))
(evil-terminal-cursor-changer-activate) ; or (etcc-on)
)

    ;make menus compact, good on Termux!
;; Menu-bar disabled in early-init.el
(setq tmm-mid-prompt '":") ; Menu Help prompt
  "String to insert between shortcut and menu item.
If nil, there will be no shortcuts.  It should not consist only of spaces,
or else the correct item might not be found in the `*Completions*' buffer."
  ;; :type 'string)
(setq tmm-completion-prompt nil)
  "Help text to insert on the top of the completion buffer.
 To save space, you can set this to nil,
 in which case the standard introduction text is deleted too."

        (use-package tabulated-list
; Make Packages buffer more compact ;
; tabulated-list sucks and it's so stiff
    :config
(setq package-name-column-width 15
      package-status-column-width 2
      package-version-column-width 1
      package-archive-column-width 1))

        (use-package doom-modeline
    :hook (after-init . doom-modeline-mode)
    :init 
(setq doom-modeline-time nil) 
;; (setq inhibit-compacting-font-caches t)
;; (setq find-file-visit-truename t)
(setq doom-modeline-buffer-encoding 'nondefault)
    :config
    ;; custom clock:
(setq doom-modeline-display-misc-in-all-mode-lines t)
;; (setq mode-line-misc-info '(:eval (emacs-uptime "%h:%m:%s")))

(setq mode-line-misc-info '(:eval 
(propertize (emacs-uptime "%.2mm") 'face
  (if (doom-modeline--active)
   '(:background "black" :foreground "white" :weight bold)
  ; '(:background "green" :foreground "white" :weight bold)
  '(:background "#000000" :foreground "#0000AA" :weight light)))))

(setq mode-line-compact 'long)
(setq doom-modeline-window-width-limit 40))

      	(use-package mode-line-bell
    :defer 1
    ; tackful modeline flash whenever bell rings ;
    :after doom-modeline
    :config (mode-line-bell-mode))

      	(use-package evil-goggles
    :config
(setq evil-goggles-duration 0.250)
(evil-goggles-mode)
(set-face-attribute 'evil-goggles-delete-face nil
:background "red")
(set-face-attribute 'evil-goggles-paste-face nil
:background "green")
(set-face-attribute 'evil-goggles-yank-face nil
:background "yellow")
)

        (use-package evil-anzu
         ;number of searches;
    :after evil-goggles
    :config (global-anzu-mode 1))
    
        (use-package simple
    :ensure nil
        ; wrap lines per word globally ;
    :config (global-visual-line-mode 1)
;; Indentation can insert tabs if this is non-nil.
(setq-default indent-tabs-mode nil)) 

        (use-package hl-line
    :config
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil
     ;; :foreground "white"
     :background "#202020"
     ;; :background "#333333"
     ;; :foreground "#FFFFFF"
     ;; :background "white"
))

	(use-package auto-dim-other-buffers
      	; self-explainatory ;
    :config
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))))

;; Centered Cursor mode https://two-wrongs.com/centered-cursor-mode-in-vanilla-emacs.html
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time

(setq scroll-preserve-screen-position t
      ;; scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 9999)
;; (setq scroll-conservatively 10000)

(setq echo-keystrokes 0.1) ; show key-combos quickly

; Excepition of above for term ;
(add-hook 'term-mode-hook (lambda()
(dolist (v (list 'scroll-preserve-screen-position 
        'scroll-conservatively
        'maximum-scroll-margin
        'scroll-margin))
(setq auto-window-vscroll t) ; (lags?)
    (local-reset-symbol v))))
(setq confirm-kill-processes nil) ; because my bash term threw errors on exit
(defmacro local-reset-symbol (s)
  `(progn (make-local-variable ,s)
          (set ,s (eval (car (get ,s 'standard-value))))))

        (use-package minibar
    :defer 3
    ; date in the minibuffer centre ;
    :config (minibar-mode t))

        (use-package window
; slapped this in but had no time to check it out yet
    :config
(setq switch-to-buffer-obey-display-actions t
display-buffer-alist
'(("\\*e?shell\\*"
    (display-buffer-in-side-window)
    (window-height . 0.40)
    (side . bottom)
    (slot . -1))
    ("\\*eldoc\\*"
    (display-buffer-in-side-window)
    (window-height . 0.30)
    (side . bottom)
    (slot . -1))
    ("\\*\\(less-css-compilation\\|compilation\\)\\*"
    (display-buffer-no-window))
    ("\\*\\(ansi-term\\|term\\)\\*"
    (display-buffer-in-side-window)
    (window-height . 0.40)
    (side . bottom)
    (slot . -1))
    ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
    (display-buffer-in-side-window)
    (window-height . 0.25)
    (side . bottom)
    (slot . 0))
    ("\\*\\([Hh]elp\\|Apropos\\)\\*"
    (display-buffer-in-side-window)
    (window-height . 0.40)
    (side . bottom)
    (slot . 0)))))

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Termux:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
(global-set-key  (kbd "M-|") `tmm-menubar)
; because shift-f10 rarely works on Termux keybs
(global-set-key [tab-bar double-mouse-1] 'tmm-menubar)


;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- MS-Windows:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

        (use-package parrot
	;key moodlifter <3;
    :if window-system
    :config (parrot-mode)
(setq parrot-num-rotations nil))
 
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Closing:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

; personal organiser I want to see on startup ;
;; (find-file "~/xinfu/todo.md")

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 9 1000 1000))
;; (message "----- Init file loaded!")
