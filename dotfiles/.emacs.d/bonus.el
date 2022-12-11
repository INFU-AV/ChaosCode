(defun split-and-follow-horizontally ()
	"Split and follow horizontally."
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))
 (global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

 (defun split-and-follow-vertically ()
	"Split and follow vertically."
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))
 (global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;;;;;;;;;;;;xahutils-start

(defun xah-comment-dwim ()
    "Like `comment-dwim', but toggle comment if cursor is not at end of line.
URL `http://xahlee.info/emacs/emacs/emacs_toggle_comment_by_line.html'
Version 2016-10-25"
(interactive)
(if (region-active-p)
    (comment-dwim nil)
 (let (($lbp (line-beginning-position))
	($lep (line-end-position)))
  (if (eq $lbp $lep)
	(progn
	    (comment-dwim nil))
	    (if (eq (point) $lep)
		(progn
		    (comment-dwim nil))
		(progn
		(comment-or-uncomment-region $lbp $lep)
		    (forward-line )))))))

(defun xah-copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the current or marked files.
If a buffer is not file and not dired, copy value of `default-directory'.
URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))

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
(setq infu-bionic-reading-face 'error)
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

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.
URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))
(provide 'xah-space-to-newline)


;;;;;;;;;;;;xahutils-end

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
( define-key evil-visual-state-map (kbd "<") '+evil/shift-left)
( define-key evil-visual-state-map (kbd ">") '+evil/shift-right)
( define-key evil-visual-state-map (kbd "gp") '+evil/alt-paste)

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

;;;;;;;;;;;;unused below:

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

;; 	(use-package recentf
;;     :config
;; (setq recentf-exclude '("/tmp/"
;;                         "/ssh:"
;;                         "/sudo:"
;;                         "recentf$"
;;                         "company-statistics-cache\\.el$"
;;                         ;; ctags
;;                         "/TAGS$"
;;                         ;; global
;;                         "/GTAGS$"
;;                         "/GRAGS$"
;;                         "/GPATH$"
;;                         ;; binary
;;                         "\\.mkv$"
;;                         "\\.mp[34]$"
;;                         "\\.el.gz$"
;;                         "^/var/folders\\.*"
;;                         "COMMIT_EDITMSG\\'"
;;                         ".*-autoloads\\.el\\'"
;;                         "[/\\]\\.elpa/"
;;                         "\\.avi$"
;;                         "\\.pdf$"
;;                         "\\.docx?$"
;;                         "\\.xlsx?$"
;;                         ;; sub-titles
;;                         "\\.sub$"
;;                         "\\.srt$"
;;                         "\\.ass$"
;;                         ;; ~/.emacs.d/**/*.el included
;;                         ;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
;;                         ))
;; (add-to-list 'recentf-exclude no-littering-var-directory)
;; (add-to-list 'recentf-exclude no-littering-etc-directory)
;; (recentf-mode 1))
