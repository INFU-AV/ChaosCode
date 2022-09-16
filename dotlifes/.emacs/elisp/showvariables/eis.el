
;;; eis.el --- Inspect current state          -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author:  Benjamin Kästner
;; Keywords: internal, convenience
;; Version: 0.0.2
;; Homepage: https://github.com/bkaestner/eis.el
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of Emacs.

;; eis is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:
(eval-when-compile (require 'cl-lib))

;;; Faces and customization options
(defgroup eis nil "Emacs Introspection System"
  :group 'convenience
  :group 'display)

;;;; Faces
(defface eis-command '((default . (:foreground "yellow")))
  "Face for commands/interactive functions"
  :group 'eis)

(defface eis-variable-scope-global '((default . (:foreground "green")))
  "Face for global variables."
  :group 'eis)

(defface eis-variable-scope-buffer '((default . (:inherit warning)))
  "Face for buffer local variables."
  :group 'eis)

(defface eis-variable-scope-environment '((default . (:inherit font-lock-builtin-face)))
  "Face for environment variables."
  :group 'eis)

(defface eis-variable-scope-original '((default . (:foreground "#00bbff")))
  "Face for environment variables."
  :group 'eis)

(defface eis-variable-scope-global nil
  "Face for global variables."
  :group 'eis)

(defface eis-customization-link '((default . (:inherit custom-link)))
  "Face for links to `customize-variable-other-window'."
  :group 'eis)

;;;; Customization options
(defconst eis--text-sample-ascii "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 (){}[]-+?!"
  "ASCII sample text.")

(defcustom eis-text-sample eis--text-sample-ascii
  "Sample text for `eis-show-faces' and `eis-show-fonts'."
  :group 'eis
  :type `(choice (const :tag "ASCII sample" ,eis--text-sample-ascii)
                 string))

(defcustom eis-font-filter 'all
  "Filter for fonts on your system.

It can be either the symbol `all' for all fonts, `monospace' for
monospaced fonts, or a custom filter. The custom filter must take
a single argument (the font name) and return non-nil on fonts
that should be shown."
  :group 'eis
  :type '(choice (const :tag "All fonts" all)
                 (const :tag "Monospaced fonts" monospace)
                 function))

(defcustom eis-variable-scope-buffer-local-handling 'diff
  "How to handle `buffer-local-variables'."
  :group 'eis
  :type '(choice (const :tag "Only values that differ from the global value" diff)
                 (const :tag "Show all values regardless of their value" all)
                 (const :tag "Do not show buffer local values at all" none)))

(defcustom eis-describe-variable-function #'describe-variable
  "Function to describe a single variable.

The function must take a single symbol and should be buffer aware."
  :group 'eis
  :type '(choice (const :tag "Emacs default describe function" describe-variable)
                 function))

(defcustom eis-describe-function-function #'describe-function
  "Function to describe a single function.

The function must take a single symbol."
  :group 'eis
  :type '(choice (const :tag "Emacs default describe function" describe-function)
                 function))

(defcustom eis-describe-face-function #'describe-face
  "Function to describe a single face."
  :group 'eis
  :type '(choice (const :tag "Emacs default describe function" describe-face)
                 function))

(defcustom eis-docstring-regexp "[^.\n\r]\\{0,75\\}"
  "Regular expression for docstrings."
  :group 'eis
  :type '(choice (const :tag "Initial sentence or line (limited to 75 characters)" "[^.\n\r]\\{0,75\\}")
                 (const :tag "Initial sentence or line" "[^.\n\r]+")
                 (const :tag "Initial line" "[^\n\r]+")
                 (const :tag "Initial sentence" "[^.]+\\.")
                 (regexp :tag "Custom Regexp")))

(defcustom eis-ellipsis "…"
  "Text used for shortened strings."
  :group 'eis
  :type 'string)

;;;; Buttons
(defun eis--call-button-action (button)
  "Call stored function on BUTTON.

The button needs to have both `eis-function' and `eis-args' set.
The former shall be a function, the latter a list."
  (apply (button-get button 'eis-function)
         (button-get button 'eis-args)))

;; Base type for EIS buttons
(define-button-type 'eis-button
  'action       #'eis--call-button-action)

;; Button to the customize interface for the given variable
(define-button-type 'eis-variable-customize-button
  :supertype 'eis-button
  'eis-function #'customize-variable-other-window)

(defun eis--buffer-aware-describe-variable (scope sym)
  "Describe SYM in the context of SCOPE.

If SCOPE is '(buffer b), then SYM will be described as if `b' was
currently visited. Otherwise it will be described in the context
of the current buffer."
  (pcase scope
    (`(buffer ,b) (with-current-buffer b (funcall eis-describe-variable-function sym)))
    (_ (funcall eis-describe-variable-function sym))))

;; Button to show variable.
(define-button-type 'eis-variable-documentation-button
  :supertype 'eis-button
  'eis-function #'eis--buffer-aware-describe-variable)

(define-button-type 'eis-function-documentation-button
  :supertype 'eis-button
  'eis-function (lambda (sym) (funcall eis-describe-function-function sym)))

(define-button-type 'eis-face-documentation-button
  :supertype 'eis-button
  'eis-function (lambda (sym) (funcall eis-describe-face-function sym)))

(define-button-type 'eis-buffer-button
  :supertype 'eis-button
  'face 'eis-variable-scope-buffer
  'eis-function (lambda (b) (pop-to-buffer b)))

;;;; Macros and base mode
;; Base Mode
(define-derived-mode eis-tabulated-list-mode tabulated-list-mode "Eis:Base"
  "Base mode for `eis' modes.")

;; Definer for new Eis modes
(cl-defmacro define-eis-mode (name &rest args)
  "Create a new EIS mode named eis-NAME-mode and an eis-show-NAME function.

Other than NAME this macro takes the following keyword arguments in ARGS:

  :columns COLUMN-SPEC - column specification, see `tabulated-list-format'
  :sort-column SCOLUMN - which column should be used for sorting by default
  :data-func DATA-FUNC - function to fill `tabulated-list-entries'

  BODY                 - form that returns `tabulated-list-entries'.

Exactly one of DATA-FUNC or BODY must be non-nil and calculate the `tabulated-list-entries'."
  (let* ((data-func (plist-get args :data-func))
         (sort-column (plist-get args :sort-column))
         (columns (plist-get args :columns))
         (sname (symbol-name name))
         (mode  (concat "eis-" sname "-mode")))
    (dolist (key '(:data-func :sort-column :columns))
      (cl-remf args key))
    (unless (xor args data-func)
      (error "Need to speficy either data-func or provide code, not both"))
    (unless data-func
      (setq data-func (append (list 'lambda 'nil) args)))
    `(eval-when-compile
       (define-derived-mode ,(intern mode) eis-tabulated-list-mode ,(concat "Eis:" (capitalize sname))
         (setq tabulated-list-format ,columns)
         (setq tabulated-list-padding 2)
         (setq tabulated-list-entries ,data-func)
         ,(when sort-column
            `(setq tabulated-list-sort-key (cons ,sort-column nil))))

       (defun ,(intern (concat "eis-show-" sname)) ()
         ,(concat "Show known " sname ".")
         (interactive)
         (pop-to-buffer ,(concat "*" sname "*"))
         (,(intern mode))
         (tabulated-list-init-header)
         (tabulated-list-print t)))))

;;;; Generic helpers
(defun eis--match-string (string regexp &optional ellip)
  "Return the match of REGEXP in STRING.

Append ELLIP to the result if REGEXP does not match the whole string."
  (setq string (string-trim string))
  (let ((newstring (or (and (string-match regexp string)
                            (match-string 0 string))
                       string)))
    (if (string= string newstring)
        string
      (concat newstring (or ellip eis-ellipsis)))))

(defun eis--elt< (a b)
  "Compare the first elements within A's and B's cadr case-insensitive."
  ;; (label [name _])
  (setq a (elt (cadr a) 0))
  (setq b (elt (cadr b) 0))
  (string< (downcase (if (listp a) (car a) a)) (downcase (if (listp b) (car b) b))))

(defun eis--ensure-single-line (string &optional ellip)
  "Ensure that STRING spans only a single line.

Append ELLIP to the result if the string spans multiple lines."
  (eis--match-string string "[^\n\r]+" ellip))

(defun eis--format (fmt &rest args)
  "Format ARGS according to FMT on a single line."
  (eis--ensure-single-line (apply #'format fmt args)))

(defun eis--format-value (value)
  "Format the given VALUE."
  (cond
   ((byte-code-function-p value) "f#(compiled function)")
   (t (eis--format "%s" value))))

(defun eis--shorten-docstring (docstring &optional ellip)
  "Shorten DOCSTRING according to `eis-docstring-regexp'.

A shortened DOCSTRING is signalized by an appended \"...\" or ELLIP if it was specified."
  (eis--match-string docstring eis-docstring-regexp ellip))

;;;; Variables
(defun eis--variable-entry (sym value &optional scope)
  "Create an entry for SYM with value VALUE within SCOPE.

SCOPE can be either
- a cons cell `(buffer . b)' for variables local to the buffer B,
- 'environment for environment variables,
- 'original for original variable values,
- nil for global values."
  (let ((name (cond
               ((symbolp sym) (symbol-name sym))
               ((stringp sym) sym)
               (t (format "%s" sym)))))
    (list
     (cons scope sym)
     (vector
      (cons name `(:type eis-variable-documentation-button eis-args (,scope ,sym)))
      (if (and (not scope) (custom-variable-p sym))
          `("customize" . (:type eis-variable-customize-button eis-args (,sym) face custom-link))
        "")
      (pcase scope
        (`(buffer . ,b) `(,(buffer-name b) . (:type eis-buffer-button eis-args (,b))))
        ('environment   '("Environment"    . (face eis-variable-scope-environment)))
        ('original      '("Original"       . (face eis-variable-scope-original)))
        ('nil           '("Global"         . (face eis-variable-scope-global))))
      (eis--format-value value)
      (eis--shorten-docstring (or (documentation-property sym 'variable-documentation) ""))))))

(defun eis--get-symbol-as-variable (symbol)
  "Get information about SYMBOL.

Return up to two `eis--variable-entry' compatible entries, one
for the global value and one for the original one, if it exists
and differs from the current one."
  (when (default-boundp symbol)
    (let* ((sv (get symbol 'standard-value))
           (defval (default-value symbol))
           (origval (and (consp sv)
                         (condition-case nil
                             (eval (car sv) t)
                           (error :eis-eval-error)))))
      (append (list (eis--variable-entry symbol defval))
              (when (and (consp sv)
                         (not (equal origval defval))
                         (not (equal origval :eis-eval-error)))
                (list (eis--variable-entry symbol origval 'original)))))))

(defun eis--calculate-variables ()
  "Calculate the entries for `eis-show-variables'."
  (append
   ;; check all global variables
   (cl-loop for v being the symbols
            when (and (boundp v) (documentation-property v 'variable-documentation))
            append (eis--get-symbol-as-variable v))
   ;; check all buffer-local variables
   (cl-loop unless (eq eis-variable-scope-buffer-local-handling 'none)
            for b being the buffers
            unless (string-match-p "^ " (buffer-name b))
            append (cl-loop for (symbol . value) in (buffer-local-variables b)
                            when (or
                                  (and (eq 'diff eis-variable-scope-buffer-local-handling)
                                       (and (default-boundp symbol)
                                     (not (equal value (default-value symbol)))))
                                  (eq 'all eis-variable-scope-buffer-local-handling))
                            collect (eis--variable-entry symbol value (cons 'buffer b))))
   ;; environment variables
   (cl-loop for e in process-environment
            do (string-match "^\\([^=]+\\)=\\(.*\\)$" e)
            collect (let ((v (match-string 1 e))
                          (val (match-string 2 e)))
                      (eis--variable-entry (intern v) val 'environment)))))

;;;###autoload (autoload 'eis-show-variables "eis.el" nil t)
(eval-when-compile
  (define-eis-mode variables
    :columns [("Variable" 40 eis--elt<)
              ("Customize" 10)
              ("Scope" 30 t)
              ("Value" 40 t)
              ("Documentation" 80 t)]
    :sort-column "Variable"
    (eis--calculate-variables)))

;;;; Functions
(defun eis--function-args (sym)
  "Return function arguments for SYM."
  (or
   (elisp-function-argstring
    (when-let ((args (help-function-arglist sym)))
    (if (and (stringp args)

             (string-match-p "Arg list not available" args))
         "[autoload]"
      args)))
   ""))

;;;###autoload (autoload 'eis-show-functions "eis.el" nil t)
(eval-when-compile
  (define-eis-mode functions
    :columns [("Function" 40 eis--elt<)
              ("Arguments" 30 nil)
              ("Documentation" 80 t)]
    :sort-column "Function"
    (cl-loop for f being the symbols
             when (fboundp f)
             collect `(,f [(,(symbol-name f) . ,(if (commandp f) '(face eis-command)))
                           ,(eis--function-args f)
                           ,(eis--shorten-docstring (or (ignore-errors (documentation f)) ""))]))))

;;;; Faces
;;;###autoload (autoload 'eis-show-faces "eis.el" nil t)
(eval-when-compile
  (define-eis-mode faces
    :columns `[("Face" 40 t)
               ("Sample" ,(1+ (length eis-text-sample)))
               ("Customize" 10)
               ("Documentation" 80)]
    :sort-column "Face"
    (cl-loop for face being the symbols
             when (facep face)
             collect `(,face
                       [(,(face-name face) . (:type eis-face-documentation-button eis-args (,face)))
                        (,eis-text-sample . (face ,face))
                        ("customize" . (:type eis-button
                                              eis-function customize-face-other-window
                                              eis-args     (,face)))
                        ,(eis--shorten-docstring (or (documentation-property face 'face-documentation) ""))]))))

;;;; Fonts
;;;###autoload (autoload 'eis-show-fonts "eis.el" nil t)
(eval-when-compile
  (define-eis-mode fonts
    :columns [("Font" 40 t)
              ("Sample" 65 nil)]
    :sort-column "Font"
    (let ((fonts (pcase eis-font-filter
                   ('all (font-family-list))
                   ('monospace (mapcar (lambda (fe)
                                         (symbol-name (font-get fe :family)))
                                       (list-fonts (font-spec :spacing 100))))
                   (f (seq-filter f (font-family-list))))))
      (cl-loop for font in (cl-remove-duplicates fonts)
               collect `(,font
                         [,font
                          (,eis-text-sample . (face (:inherit default :family ,font)))])))))

(provide 'eis)
;;; eis.el ends here
