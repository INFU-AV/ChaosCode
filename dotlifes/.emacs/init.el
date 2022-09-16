;; -*- lexical-binding: t; -*-

;; (setq user-full-name "INFU")

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
; - - FORMARRING WARNING - -
;I format packages in way that
; long lines are least idented 
;for better readibility on					
; phones & thin small screens 
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;
;    .dotfiles to learn from:
; https://github.com/daviwil/emacs-from-scratch
; https://github.com/daviwil/dotfiles/blob/master/Emacs.org
; https://github.com/SqrtMinusOne/dotfiles/blob/master/Emacs.org
; https://taingram.org/init.html
; https://sachachua.com/dotemacs/index.html
; https://github.com/DiamondBond/emacs/blob/master/config.org
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
;; ----- Beauty/visibility:
;; ----- Dired-stuff:
;; ----- Markdown-stuff:
;; ----- Functionality:
;; ----- Closing:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;


;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Crucial:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

;; ===== TROUBLESHOOTING =====
(defun infu/display-startup-time ()
 "Personalized Yak-shaving startup timer."
    (message
">->->[LOADING TIME >>> %s]<-<-<
 >->-> GARBAGE COLLECTED -> (%.2d) <-<-<"
 (format "%.3f sec!"
    (float-time
 (time-subtract after-init-time before-init-time)))
    gcs-done))
(add-hook 'emacs-startup-hook #'infu/display-startup-time)

    	(require 'package)
    ;; Initialize package sources ;;

(setq package-archives
    '(("melpa"  . "https://melpa.org/packages/")
      ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
      ("elpa"   . "https://elpa.gnu.org/packages/")
      ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (package-installed-p 'use-package)
(package-install 'use-package))

    	(require 'use-package)


    ; Debugging:		
;; (setq debug-on-error t) ; / M-x "toggle-debug-on-error"
;; M-x "redraw-display" should get rid of visual glitches
;; M-x "memory-report" to see summary of
;; ;;    for bootup-time analysis:
;; (setq use-package-compute-statistics 1)
; M-x "use-package-report"  to see stats from function above
;; (setq use-package-verbose t) ;; see what packages get loaded in detail

    ;; Set default custom file
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

;; If custom.el doesn't exist, packages probably too!
;; my sneaky way of deploying from scratch
(unless (file-exists-p custom-file)
(unless package-archive-contents
  (package-refresh-contents))
(setq use-package-always-ensure t)
)
    ;; Write to it if it does not exist
(unless (file-exists-p custom-file)
 (write-region "" nil custom-file))
    ;; Load custom file. Don't hide errors. Hide success message
(load custom-file nil t)
    ;; previously used those:
 ;; (setq custom-file (concat user-emacs-directory "custom.el"))
 ;; (load custom-file 'noerror)

; Do not steal focus while doing async compilations
;; (setq warning-suppress-types '((comp)))

      	(use-package no-littering
        ;makes sure other packages won't make mess;
    :init
(setq no-littering-etc-directory
 (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(setq auto-save-file-name-transforms
 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Save backup files to a dedicated directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq kept-new-versions 5)
(setq kept-old-versions 2)

;; Make numeric backup versions unconditionally.
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Do not create lock files.
(setq create-lockfiles nil)
(setq auto-save-timeout 2)
(auto-save-visited-mode +1)
)

    	(use-package evil
	;vim-style modal keybinding & workflow;
    ;; Manuals:
;; https://evil.readthedocs.io
;; https://github.com/noctuid/evil-guide
    :init
(setq  evil-move-beyond-eol t)
(setq  evil-undo-system 'undo-fu)
(setq  evil-want-C-i-jump nil)
(setq evil-esc-delay 0.02)
(setq  evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq  evil-want-keybinding nil)
(setq  evil-auto-indent nil)
(setq  evil-move-cursor-back nil)
(setq  evil-kill-on-visual-paste nil)
    :config
;; (setq  evil-search-module 'evil-search)
(setq  evil-flash-delay 2)
(setq evil-ex-hl-update-delay 0.5)
(evil-mode))

    	(use-package evil-collection
	;more cool bindings for evil;
    ;; :defer 1
    :after evil
    :init
(setq annalist-record nil) ; for recording information, don't need it
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
	    hungry-delete
	    ibuffer
	    imenu
	    imenu-list
	    (indent "indent")
	    markdown-mode
	    ,@(when evil-collection-setup-minibuffer '(minibuffer))
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
	    (ztree ztree-diff ztree-dir))))

        (use-package undo-fu
    :config
(setq undo-limit 3355443200) ;; 32mb.
;; (setq undo-strong-limit 100663296) ;; 96mb.
;; (setq undo-outer-limit 1006632960) ;; 960mb.
)

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Navigation/KeyCombos:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

(xterm-mouse-mode t)
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)
(global-set-key  (kbd "M-<f10>") `context-menu-open) ; because shift-f10 rarely works on Termux keybs
(global-subword-mode 1) 
(setq confirm-kill-processes nil)


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
(define-key infu-map (kbd "E") 'eval-buffer)
(define-key infu-map (kbd "p") 'list-packages)
(define-key infu-map (kbd "SPC") 'eval-region)

    ;; Ease of life functions
(define-key infu-map (kbd "y") 'yank-from-kill-ring) 
(define-key infu-map (kbd "(") 'check-parens) 
(define-key infu-map (kbd ",") 'evil-window-next)
(define-key infu-map (kbd "X") 'copy-to-register)
(define-key infu-map (kbd "x") 'insert-register)
(define-key infu-map (kbd "Z") 'kmacro-start-macro)
(define-key infu-map (kbd "z") 'kmacro-end-or-call-macro)
(define-key infu-map (kbd "f") 'find-file)
(define-key infu-map (kbd "F") 'find-file-other-tab)
(define-key infu-map (kbd "b") 'switch-to-buffer)
(define-key infu-map (kbd "B") 'switch-to-buffer-other-tab)
(define-key infu-map (kbd "k") 'kill-buffer)
(define-key infu-map (kbd "K") 'kill-buffer-and-window)
(define-key infu-map (kbd "S") 'follow-delete-other-windows-and-split)
(define-key infu-map (kbd "C-k") 'tab-close)
(define-key infu-map (kbd "n") 'dired) ;for {n}avigation
 ; Rewrite this one as relative path:
(define-key infu-map (kbd "A") (lambda() (interactive)(find-file "~/.emacs.d/elisp/my-abbrev.el"))) 
(define-key infu-map (kbd "t") 'tab-bar-new-tab)
;; (define-key infu-map (kbd "t") (lambda() (interactive)(find-file "~/xinfu/todo.md")))
(define-key infu-map (kbd "x") 'hs-minor-mode)
(define-key infu-map (kbd "1") 'delete-other-windows)
(define-key infu-map (kbd "C-m") 'bookmark-bmenu-list)
(define-key infu-map (kbd "m") 'bookmark-jump)
(define-key infu-map (kbd "M") 'bookmark-set)
;; since normal-mode "m" is marking things, we go back to them with
(define-key infu-map (kbd "v") 'evil-goto-mark) 
(define-key infu-map (kbd "!") 'delete-window)
(define-key infu-map (kbd "j") 'term)
(define-key infu-map (kbd "2") 'split-and-follow-horizontally)
(define-key infu-map (kbd "3") 'split-and-follow-vertically)
;; (define-key infu-map (kbd "3") 'split-window-right)
(define-key infu-map (kbd "<prior>") 'scroll-other-window-down)
(define-key infu-map (kbd "<next>") 'scroll-other-window)
(define-key infu-map (kbd "TAB ") 'markdown-shifttab) ;; because s-<TAB> doesn't work on termux lmao

;; ( define-key evil-normal-state-map (kbd "l") "")

	(use-package hydra
	;more key combos;
	;; :defer 1
    :config
    ;; (setq hydra-lv t)
    ;; (setq lv-use-separator t)
  (defhydra hydra-window ( :columns 4 :hint nil )
    "Resizing"
   ("<down>"  evil-window-decrease-height "↓")
   ("<left>"  evil-window-decrease-width "←")
   ("<right>" evil-window-increase-width "→")
   ("<up>"	   evil-window-increase-height "↑")
   (":"       delete-window "" :color blue )
   ("1"       delete-other-windows "☠" :color blue )
   ("2"       split-window-below "✂" :color blue )
   ("3"       split-window-right "✄" :color blue )
   ("8"       balance-windows "⚖" :color blue ))
(define-key infu-map (kbd "W") 'hydra-window/body)
(defhydra hydra-tab-bar (:color amaranth)
  "Tab Bar Operations:"
  ("t" tab-new "Just new" :column "New Tab")
  ("d" dired-other-tab "Dired")
  ("f" find-file-other-tab "Find file")
  ("m" tab-move "Move it" :column "This Tab")
  ("r" tab-rename "Rename it")
  ("0" tab-close "Close it")
  ;; ("<return>" tab-bar-select-tab-by-name "Select tab by name" 
  ("<right>" tab-next "Tab ->" :column "Navigation")
  ("<left>" tab-previous "<- Tab")
  ("q" nil "Exit" :exit t))
(define-key infu-map (kbd "w") 'hydra-tab-bar/body)
)

        (use-package abbrev
    :ensure nil
    ;; :defer 1
;; abbrev-mode xah style http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
    :config
; Fix the path, also figure out path-free combo to reach it
(setq abbrev-file-name "~/.emacs.d/elisp/my-abbrev.el")
;; (setq abbrev-file-name "/data/data/com.termux/files/home/.emacs.d/elisp/my-abbrev.el")
)

        (use-package ido
	;; :ensure nil
    :init
(setq ido-separator "\n")
;; (setf (nth 2 ido-decorations) "\n")
;; show any name that has the chars you typed
(setq ido-enable-flex-matching t)
;; use current pane for newly opened file
(setq ido-default-file-method 'selected-window)
;; use current pane for newly switched buffer
(setq ido-default-buffer-method 'selected-window)
    :config
      (ido-mode 1))

	(use-package icomplete 
    ;; show choices vertically
	;; :ensure nil
    :defer 1
    :init
(setq icomplete-separator "\n")
(setq icomplete-hide-common-prefix nil)
(setq icomplete-in-buffer t)
    :config
(define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "M-<RET>") 'icomplete-force-complete-and-exit)
(icomplete-mode 1)
)

        (use-package savehist
    ;; :ensure nil
    :config (savehist-mode 1))
 

        (use-package isearch
    :ensure nil)

	(use-package auto-complete
    :config
(ac-config-default)
(auto-complete-mode 1))

	(use-package orderless
    :init
(setq completion-styles '(orderless basic)
completion-category-defaults nil
completion-category-overrides '((file (styles basic partial-completion)))))

	(use-package recentf
    ;; :ensure nil
    :defer 1
    :config
(setq recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "recentf$"
                        "company-statistics-cache\\.el$"
                        ;; ctags
                        "/TAGS$"
                        ;; global
                        "/GTAGS$"
                        "/GRAGS$"
                        "/GPATH$"
                        ;; binary
                        "\\.mkv$"
                        "\\.mp[34]$"
                        "\\.el.gz$"
                        "^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"
                        "\\.avi$"
                        "\\.pdf$"
                        "\\.docx?$"
                        "\\.xlsx?$"
                        ;; sub-titles
                        "\\.sub$"
                        "\\.srt$"
                        "\\.ass$"
                        ;; ~/.emacs.d/**/*.el included
                        ;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
                        ))
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(recentf-mode 1))

    ;; to not overload my init.el with
    ;; tons blocks of code, i split it here:
(load "~/.emacs.d/bonus.el")

    ;; xah-comment-dwim
(define-key evil-normal-state-map (kbd "g c") 'xah-comment-dwim)
    ;; xah-beginning-of-line-or-block
    ;; xah-end-of-line-or-block
(define-key evil-motion-state-map (kbd "<home>") 'xah-beginning-of-line-or-block)
(define-key evil-motion-state-map (kbd "<end>") 'xah-end-of-line-or-block)
    ;; xah-punctuation-regex
(define-key evil-motion-state-map (kbd "C-<home>") 'xah-backward-punct)
(define-key evil-motion-state-map (kbd "C-<end>") 'xah-forward-punct)
    ;; xah-copy-file-path
; M-x ^
    ;; xah-dired-sort
(evil-define-key 'normal dired-mode-map (kbd "s") 'xah-dired-sort)

        (use-package infu-bionic-face
    :load-path "/elisp/infu-bionic-face"
    :ensure nil
    :commands (infu-bionic-reading-buffer - infu-bionic-reading-region))
 
        (use-package xah-space-to-newline
    :load-path "/elisp/xah-space-to-newline"
    :ensure nil
    :commands (xah-space-to-newline))

        (use-package winner
	      ; undo-window changes ;
	;; :diminish windmove-mode
    :config (winner-mode t))

        (use-package delsel
	      ; replace selection ;
    :config (delete-selection-mode 1))

	(use-package move-text
	; move lines up/down ;
    ;; :config (move-text-default-bindings))
    :bind
("ESC <up>" . move-text-up)
("M-<up>" . move-text-up)
("ESC <down>" . move-text-down)
("M-<down>" . move-text-down))


;; Multiplayer emacs packages
  (use-package impatient-mode  :disabled)
  (use-package impatient-showdown-mode  :disabled)
  (use-package crdt  :disabled)


;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Dired-stuff:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;


	(use-package dired
    :ensure nil
    ;; :hook (dired-mode . 
    :config 
(evil-define-key 'normal 'dired-mode-map
(kbd "C-n") 'dired-create-empty-file
(kbd "C-N") 'dired-create-directory)
(setq dired-kill-when-opening-new-dired-buffer t) ; NEW in Emacs 28
(setq dired-dwim-target t) ; suggest file-path of another open Dired buffer if there is one
(setq dired-hide-details-mode 1)
    :custom
((dired-listing-switches "-agho --group-directories-first")))

	(use-package all-the-icons-dired
	;decorates files with stylish icons;
    :after dired
    :hook (dired-mode . all-the-icons-dired-mode))

	(use-package diredfl
    :after dired
    :hook (dired-mode . diredfl-mode))

        (use-package autorevert
    ;; :diminish auto-revert-mode
    :config
(setq global-auto-revert-non-file-buffers t) ; autoupdate Dired
(global-auto-revert-mode 1)) ; autoupdate files


;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Markdown-stuff:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

 
	      (use-package markdown-mode
    :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown")
(setq markdown-reference-location "immediately")
(setq markdown-url-compose-char "°")
(setq markdown-hide-urls t)
    :config
(set-face-attribute 'markdown-header-face-1 nil
    :foreground "#FF4444"
    :background "#000000"
    :weight 'ultra-bold
    :overline "white"
    :slant 'italic
    :underline "white")
(set-face-attribute 'markdown-header-face-2 nil
    :foreground "#FFFF00"
    :background "#003300"
    :weight 'ultra-bold
    :slant 'oblique
    :underline 't)
(set-face-attribute 'markdown-header-face-3 nil
    :foreground "#44aaff"
    :background "#000033"
    :weight 'bold
    :slant 'normal
    :underline 't)
(set-face-attribute 'markdown-header-face-4 nil
    :foreground "#00ff00"
    :background "#003300"
    :weight 'ultra-bold
    :slant 'normal
    :underline 't)
(set-face-attribute 'markdown-header-face-5 nil
    :background "#330033"
    :weight 'bold
    :slant 'reverse-oblique
    :underline 't)
(set-face-attribute 'markdown-header-face-6 nil
    :weight 'bold
    :underline 't)
)

     	(use-package zk
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

	      (use-package zk-index
    :after zk
    :config (zk-index-setup-embark)
    :custom (zk-index-desktop-directory zk-directory))

	    (defvar zk-index-mode-map
    (let ((map (make-sparse-keymap)))
(define-key map (kbd "n") #'zk-index-next-line)
(define-key map (kbd "p") #'zk-index-previous-line)
(define-key map (kbd "v") #'zk-index-view-note)
(define-key map (kbd "o") #'other-window)
(define-key map (kbd "f") #'zk-index-focus)
(define-key map (kbd "s") #'zk-index-search)
(define-key map (kbd "d") #'zk-index-send-to-desktop)
(define-key map (kbd "D") #'zk-index-switch-to-desktop)
(define-key map (kbd "c") #'zk-index-current-notes)
(define-key map (kbd "i") #'zk-index-refresh)
(define-key map (kbd "S") #'zk-index-sort-size)
(define-key map (kbd "M") #'zk-index-sort-modified)
(define-key map (kbd "C") #'zk-index-sort-created)
(define-key map (kbd "RET") #'zk-index-open-note)
(define-key map (kbd "q") #'delete-window)
    (make-composed-keymap map tabulated-list-mode-map))
  "Keymap for ZK-Index buffer.")


;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Functionality:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

      	(use-package xclip
	; Android clipboard integration
    :if (eq system-type 'gnu/linux)
    :config (xclip-mode 1))

; ↓ setting default browser so
; ↓ Emacs asks which one to use per link
; ↓ Android-specific, requires "termux-api"
    (advice-add 'browse-url-default-browser :override
(lambda (url &rest args)
(start-process-shell-command "open-url" nil (concat "am start -a android.intent.action.VIEW -d " url))))
(global-set-key [mouse-2] #'ffap-at-mouse)

        (use-package electric
	; turn off all indents as I do my own here ;
    :config (electric-indent-mode 0))

        (use-package saveplace
	; remember cursor position in files ;
    ;; :diminish save-place-mode
    :config (save-place-mode 1))

      	(use-package hideshow)

	      (use-package command-log-mode
	;log commands you typed in;  
    :commands (command-log-mode))

      	(use-package decide
	;d20 throw randomizer;
    :commands (decide-mode))

      	(use-package ca65-mode
	    ;NES syntax;  
    :commands (ca65-mode))

      	(use-package free-keys
	 ;look for free keys;
    :commands (free-keys))

	(use-package eis
    ;; :ensure nil
    :load-path "elisp/showvariables"
    :commands
(eis-show-faces eis-show-fonts eis-show-functions eis-show-variables))

        (use-package tab-bar
    :config (tab-bar-mode)
;; (tab-bar-history-mode)
(define-key evil-normal-state-map (kbd "g C-t") 'tab-bar-new-tab)
(define-key evil-normal-state-map (kbd "T") 'tab-bar-switch-to-prev-tab)
(define-key evil-normal-state-map (kbd "t") 'tab-bar-switch-to-next-tab)
;; (define-key evil-normal-state-map (kbd "g C-t") 'tab-bar-new-tab)
(define-key evil-normal-state-map (kbd "g M-t") 'tab-close)
)

; giving sosme kind of usefulness to scratch
(setq initial-scratch-message (current-time-string))

	(use-package tooltip
    :ensure nil
    :config (setq tooltip-mode nil))
 
        (use-package cycle-themes
; Forked version of very old MELPA package
; https://github.com/runejuhl/cycle-themes.el
; managed to find it in github "issues" of MELPA's package lmao
    :load-path "elisp/cycle-themes"
    :config
; to work, requires having any theme to be loaded first
(cycle-themes-mode 1) 
:bind ("C-c C-t" . cycle-themes))

;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;; ----- Beauty/visibility:
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
;;-;;-;;-;;-;;-;;-;;-;;-;;-;;

      	(use-package evil-terminal-cursor-changer
	;first cosmetics I installed, beautify cursors;  
    :load-path "elisp/evil-terminal-cursor-changer-final"
	;using version 7696122 committed on 25 Dec 2021; 
    ;; :pin manual
    :config 
    (setq evil-insert-state-cursor '("blue" hollow)
    evil-emacs-state-cursor '("magenta" hbar)
    evil-normal-state-cursor '("green" box)
    evil-visual-state-cursor '("orange" box)
    evil-operator-state-cursor '("red" hbar)
    evil-replace-state-cursor '("magenta" hollow)
    evil-motion-state-cursor '("yellow" box))
(evil-terminal-cursor-changer-activate) ; or (etcc-on)
)

      	(use-package parrot     :disabled  
	;key moodlifter <3 (not in terminal tho)
    :if window-system
    :config (parrot-mode)
(setq parrot-num-rotations nil))

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
(setq package-version-column-width 1
      package-name-column-width 20
      package-status-column-width 2
      package-archive-column-width 1)
 (setq-default tabulated-list-format
        ;; The sum of column width is 80 characters:
    [("Package" 1 t)
    ("Status" 1 t)
    ("Last Event" 1 t)]))

	      (use-package doom-modeline
    :hook (after-init . doom-modeline-mode)
    :init 
    ;; custom clock:
(add-to-list 'global-mode-string '(:eval (emacs-uptime "%h:%.2m:%s")))
    :config
(setq mode-line-compact 'long)
(setq doom-modeline-buffer-encoding 'nondefault)
(setq doom-modeline-window-width-limit 40)
)

      	(use-package mode-line-bell
    ; tackful modeline flash whenever bell rings ;
    :after doom-modeline
    :config (mode-line-bell-mode))

      	(use-package evil-goggles
    ;; :diminish evil-goggles-mode
    ;; :after evil
    :config (evil-goggles-mode))

	      (use-package evil-anzu
         ;number of searches;
    :after evil-terminal-cursor-changer
    :config (global-anzu-mode 1))
    
        (use-package simple
        ; wrap lines per word globally ;
    :ensure nil
    ;; :diminish global-visual-line-mode
    :config (global-visual-line-mode 1)) 

        (use-package huecycle
    ;; :diminish huecycle-mode
    ; colour-flashing eye candy ;
    :defer 2
    ;; :init
    :config
    (huecycle-set-faces
;; ((background . hl-line)
((foreground . error)
    :random-color-hue-range (0.0 1.0)
    :random-color-saturation-range (0.6 0.9)
    :random-color-luminance-range (0.7 0.8)
    :speed 1.5 )
((foreground . (doom-modeline-evil-normal-state
		doom-modeline-evil-insert-state
		doom-modeline-buffer-major-mode
		line-number-current-line
		doom-modeline-lsp-success
		doom-modeline-panel
		doom-modeline-info))
    :random-color-hue-range (0.0 1.0)
    :random-color-saturation-range (0.8 1.0)
    :random-color-luminance-range (0.5 0.8))
((background . auto-dim-other-buffers-face)
    :random-color-hue-range (0.0 1.0)
    :random-color-saturation-range (0.3 0.8)
    :random-color-luminance-range (0.1 0.2))
((foreground . warning)
    :color-list ("#FF0000" "#FF0000" "#DDAAAA")
    :next-color-func huecycle-get-next-list-color
    :speed 5.0)
((foreground . region)
    :random-color-hue-range (0.0 1.0)
    :random-color-saturation-range (0.9 1.0)
    :random-color-luminance-range (0.5 0.8)))
(huecycle-when-idle 1.4))

(load-theme 'modus-vivendi)

        (use-package auto-dim-other-buffers
      	; self-explainatory ;
    ;; :init
    :config
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))))

        (use-package hl-line
    ;; :diminish global-hl-line-mode
    :init
    :config
(global-hl-line-mode 1)
 ;;    :custom
 ;; (set-face-attribute 'hl-line nil
 ;;     ;; :foreground "white"
 ;;     :foreground "black"
 ;;     :background "white")
)
 
;; Centered Cursor mode https://two-wrongs.com/centered-cursor-mode-in-vanilla-emacs.html
(setq scroll-preserve-screen-position t
      scroll-conservatively 1
      maximum-scroll-margin 0.5
      scroll-margin 999)
;; (setq scroll-conservatively 10000)
;; (setq auto-window-vscroll nil) ; (lags?)
(setq echo-keystrokes 0.1) ; show key-combos quickly


;;-;;-;;-;;-;;-;;-;;-;;-;;-;;
(message "Init file finished!")

; personal organiser I want to see on startup ;
(find-file "~/xinfu/todo.md")

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 9 1000 1000))
/
