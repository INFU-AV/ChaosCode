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
