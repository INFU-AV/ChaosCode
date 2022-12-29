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

(defun grt/scratchpad ()
  "Switch to the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

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

;;; when using emacs remotely, this crashes connection
;;         (use-package huecycle
;;     ;; :diminish huecycle-mode
;;     ; colour-flashing eye candy ;
;;     :defer 5
;;     ;; :init
;;     :config
;;     (huecycle-set-faces
;; ;; ((background . hl-line)
;; ((background . Infu-Red)
;;     :random-color-hue-range (0.0 1.0)
;;     :random-color-saturation-range (0.6 0.9)
;;     :random-color-luminance-range (0.7 0.8)
;;     :speed 1.5 )
;; ((foreground . (doom-modeline-evil-normal-state
;; 		doom-modeline-evil-insert-state
;; 		doom-modeline-buffer-major-mode
;; 		line-number-current-line
;; 		doom-modeline-lsp-success
;; 		doom-modeline-panel
;; 		doom-modeline-info))
;;     :random-color-hue-range (0.0 1.0)
;;     :random-color-saturation-range (0.8 1.0)
;;     :random-color-luminance-range (0.5 0.8))
;; ;; ((background . auto-dim-other-buffers-face)
;; ;;     :random-color-hue-range (0.0 1.0)
;; ;;     :random-color-saturation-range (0.3 0.8)
;; ;;     :random-color-luminance-range (0.1 0.2))
;; ((foreground . warning)
;;     :color-list ("#FF0000" "#FF0000" "#DDAAAA")
;;     :next-color-func huecycle-get-next-list-color
;;     :speed 5.0)
;; ((foreground . region)
;;     :random-color-hue-range (0.0 1.0)
;;     :random-color-saturation-range (0.9 1.0)
;;     :random-color-luminance-range (0.5 0.8)))
;; (huecycle-when-idle 1.4))