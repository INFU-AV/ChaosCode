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

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.
• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2017-05-13"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (equal last-command this-command ))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
            (progn
              (skip-chars-backward "\n\t ")
              (forward-char ))
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.
• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2017-05-30"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (equal last-command this-command ))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" ))
    (end-of-line)))

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

(defvar xah-punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq xah-punctuation-regex "[\\!\?\"\.'#$%&*+,/:;<=>@^`|~]+")
(defun xah-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `xah-punctuation-regex'
URL `http://xahlee.info/emacs/emacs/emacs_jump_to_punctuations.html'
Version 2017-06-26"
  (interactive "p")
  (re-search-forward xah-punctuation-regex nil t n))
(defun xah-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'
URL `http://xahlee.info/emacs/emacs/emacs_jump_to_punctuations.html'
Version 2017-06-26"
  (interactive "p")
  (re-search-backward xah-punctuation-regex nil t n))

;;;;;;;;;;;;xahutils-end

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


;; ----- TODO:
;; - Check if possible to incorporate Strokes gestures ez,
;; - Reorganise packages so they fit in logical order
;; - integrate hs-minor-mode into this init file
;; - https://www.youtube.com/watch?v=rvWbUGx9U5E&t=1s
;; - 
;; ----- Emacs new update gamechangers:
;; https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.28
;; 198 context-menu-mode
;; 289 does it change xah package?
;; 932 'copy-matching-lines' /  'kill-matching-lines'
;; 988 - save-place-file / save-place-abbreviate-file-names
;; 992 - copy-region-blink
;; 2387 - thing-at-point
;; 2437 - world-clock commands
;; 2470 - python shell interpreter
;; 3077 - removed functions

