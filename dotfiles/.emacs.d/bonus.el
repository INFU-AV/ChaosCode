;;;;;;;;;;;;xahutils-start

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://xahlee.info/emacs/emacs/dired_sort.html'
Version: 2018-12-23 2022-04-07"
  (interactive)
  (let (xsortBy xarg)
    (setq xsortBy (completing-read "Sort by:" '( "date" "size" "name" )))
    (cond
     ((equal xsortBy "name") (setq xarg "-Al "))
     ((equal xsortBy "date") (setq xarg "-Al -t"))
     ((equal xsortBy "size") (setq xarg "-Al -S"))
     ((equal xsortBy "dir") (setq xarg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other xarg )))

(defvar infu-bionic-reading-face nil "a face for `infu-bionic-reading-region'.")
(setq infu-bionic-reading-face 'bold)
;; (setq infu-bionic-reading-face 'error)
;; try
;; 'bold
;; 'error
;; 'warning
;; 'highlight
;; or any value of M-x list-faces-display
(defun infu-bionic-reading-buffer ()
  "Bold the first few chars of every word in current buffer.
Version 2022-05-21"
  (interactive)
  (infu-bionic-reading-region (point-min) (point-max)))
(defun infu-bionic-reading-region (Begin End)
  "Bold the first few chars of every word in region.
Version 2022-05-21"
  (interactive "r")
  (let (xBounds xWordBegin xWordEnd  )
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (forward-word)
        ;; bold the first half of the word to the left of cursor
        (setq xBounds (bounds-of-thing-at-point 'word))
        (setq xWordBegin (car xBounds))
        (setq xWordEnd (cdr xBounds))
        (setq xBoldEndPos (+ xWordBegin (1+ (/ (- xWordEnd xWordBegin) 2))))
        (put-text-property xWordBegin xBoldEndPos
                           'font-lock-face infu-bionic-reading-face)))))
(provide 'infu-bionic-face)

;;;;;;;;;;;;xahutils-end

(defun split-and-follow-horizontally ()
	"Split and follow horizontally."
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))

 (defun split-and-follow-vertically ()
	"Split and follow vertically."
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))

;"grt/" https://grtcdr.tn/dotfiles/emacs/
(defun grt/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun grt/kill-current-window ()
  "Kill the current buffer and window."
  (interactive)
  (grt/kill-current-buffer)
  (delete-window))

;; from Doom:
(defun +evil/shift-right ()
  "vnoremap < <gv"
  (interactive)
  (call-interactively #'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))
(defun +evil/shift-left ()
  "vnoremap > >gv"
  (interactive)
  (call-interactively #'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun +evil/alt-paste ()
  "Call `evil-paste-after' but invert `evil-kill-on-visual-paste'.
By default, this replaces the selection with what's in the clipboard without
replacing its contents."
  (interactive)
  (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
    (call-interactively #'evil-paste-after)))

(defun NoCentering()
(lambda()
"Loosens up my default cursor centering
Gifted by kind Emacs Crafters Discord user"
(dolist (v (list 'scroll-preserve-screen-position
        'scroll-conservatively
        'maximum-scroll-margin))
(setq auto-window-vscroll t) ; (lags?)
    (local-reset-symbol v)))
(setq confirm-kill-processes nil) ; because my bash term threw errors on exit
(setq maximum-scroll-margin 0.1)
(defmacro local-reset-symbol (s)
  `(progn (make-local-variable ,s)
          (set ,s (eval (car (get ,s 'standard-value)))))))

;; make sure dired buffers end in a slash so we can identify them easily
;; lifted from: https://iqss.github.io/IQSS.emacs/init.html#make_emacs_friendlier_to_newcomers
(defun ensure-buffer-name-ends-in-slash ()
  "Change buffer name to end with slash."
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)
(add-hook 'dired-mode-hook
          (lambda()
             (setq truncate-lines 1)))

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
    :background "#330033")

(copy-face 'Infu-Red 'markdown-header-face-1 )
(copy-face 'Infu-Yellow 'markdown-header-face-2 )
(copy-face 'Infu-Blue 'markdown-header-face-3 )
(copy-face 'Infu-Green 'markdown-header-face-4 )
(copy-face 'Infu-Purple 'markdown-header-face-5 )

(copy-face 'Infu-Blue 'ido-subdir )
(copy-face 'Infu-Green 'ido-only-match )
(copy-face 'Infu-Blue 'minibuffer-prompt )
(copy-face 'Infu-Green 'tab-bar-tab )
(copy-face 'Infu-Blue 'tab-bar )
(copy-face 'Infu-Blue 'tab-bar-tab-inactive )
(copy-face 'Infu-Yellow 'region)
;; (copy-face 'Infu-Green 'mode-line )
(copy-face 'Infu-Yellow 'doom-modeline-highlight)
(copy-face 'Infu-Yellow 'show-paren-match)
(set-face-attribute 'mode-line nil
    :foreground "#FFFCFF"
    :background "#330033")
(set-face-attribute 'default nil
:foreground "#FFFFe0"
:background "#220000")
(set-face-attribute 'font-lock-keyword-face nil
:foreground "#A529B0"
:bold t
:underline t)
(set-face-attribute 'font-lock-builtin-face nil
:foreground "palegreen"
:underline t)
(set-face-attribute 'font-lock-comment-face nil
:foreground "salmon"
:background "#002040")
(copy-face 'Infu-Yellow-bg 'font-lock-constant-face)
;; (set-face-attribute 'font-lock-constant-face nil
;; :background ""
;; :bold t
;; :foreground "darkgreen")
(copy-face 'Infu-Red 'font-lock-warning-face)
(set-face-attribute 'font-lock-string-face nil
:foreground "turquoise")
(copy-face 'Infu-Green 'mode-line-highlight)
(copy-face 'Infu-Blue 'mode-line-inactive)
)

;;;;;;;;;;;;unused below:

;; EdiredTerm.el:
;; (progn (dired-jump)
;; (split-window)
;; (term "bash")
;;;; (eshell)
;; (tab-bar-new-tab)
;; (find-file "~/xinfu/todo.md"))

;; EnonEmpty.el
;; ;; 2022-04-27
;; ;; open the files of command args, if they are not empty
;;
;; ;; save this file as do.el
;; ;; run it in terminal like this:
;; ;; emacs --script do.el file1 file2 etc
;; ;; written by Xah for me
;;
;; (while argv
;;   (setq xPath (pop argv))
;;   (setq xSize (nth 7 (file-attributes xPath)))
;;   (if (eq 0 xSize)
;;       nil
;;     (progn
;;       (find-file xPath))))

;; Fantastically functioning but slow'ish
    ;; (defmacro infu/save-tab-excursion (&rest body)
    ;;   "Opens a new tab in tab-line in the background and executes BODY
    ;; inside, then restores the previously selected tab."
    ;;   `(progn
    ;; 	 (tab-bar-new-tab)
    ;; 	 (unwind-protect (progn ,@body)
    ;; 	   (tab-bar-switch-to-recent-tab))))
    ;; (infu/save-tab-excursion
    ;;  (find-file "~/xinfu/todo.md"))

  ;; disabled as Im trying vertico ;;
;;         (use-package ido
;; ;; (setf (nth 2 ido-decorations) "\n")
;;     :init (setq ido-separator "\n")
;; ;; show any name that has the chars you typed
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere 1)
;;     :config (ido-mode 1))

  ;; disabled as Im trying vertico ;;
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
