;;; symbol-overlay.el --- Highlighting symbols with keymap-enabled overlays

;; Copyright (C) 2017 wolray

;; Author: wolray <wolray@foxmail.com>
;; URL: https://github.com/wolray/symbol-overlay/
;; Keywords: faces, matching
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Highlighting symbol while enabling you to jump from one occurrence to another
;; or directly to the definition of that symbol in the buffer, with A SINGLE
;; KEYSTROKE.  It was originally inspired by the package `highlight-symbol'.
;; The difference is that every symbol in `symbol-overlay' is highlighted by the
;; Emacs built-in function `overlay-put' rather than the `font-lock' mechanism
;; used in `highlight-symbol'.

;; Advantages

;; In `symbol-overlay', `overlay-put' is much faster than the traditional
;; highligting method `font-lock-fontify-buffer', especially in a large buffer
;; or even a less-than-100-lines small buffer of major-mode with complicated
;; keywords syntax such as haskell-mode.

;; More importantly, using `overlay-put' to highlight symbols has a significant
;; benefit to enabling AN AUTO-ACTIVATED OVERLAY-INSIDE KEYMAP for quick jump
;; and other useful commands.

;; You can also jump to a symbol's definition from any occurrence by
;; using `symbol-overlay-jump-to-definition', as long as the syntax of the
;; definition is specified in the buffer-local variable
;; `symbol-overlay-definition-function'.

;; All the overlays of each symbol are stored sequentially in an alist
;; `symbol-overlay-keywords-alist'.  By simply getting the current overlay's
;; index in the corresponding keyword-list as well as the length of it in the
;; alist,the number of occurrences can be immediately obtained.  While in
;; `highlight-symbol', this would call the function `how-many' twice, causing
;; extra costs.

;; Usage

;; To use `symbol-overlay' in your Emacs, you need only to bind one key:
;; (require 'symbol-overlay)
;; (global-set-key (kbd "M-i") 'symbol-overlay-put)

;;; Code:

(require 'thingatpt)
(require 'cl-lib)

(defvar symbol-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'symbol-overlay-put)
    (define-key map (kbd "u") 'symbol-overlay-jump-prev)
    (define-key map (kbd "o") 'symbol-overlay-jump-next)
    (define-key map (kbd "k") 'symbol-overlay-remove-all)
    (define-key map (kbd "d") 'symbol-overlay-jump-to-definition)
    (define-key map (kbd "q") 'symbol-overlay-query-replace)
    map)
  "Keymap automatically activated inside overlays.
You can re-bind the commands to any keys you prefer.")

(defvar symbol-overlay-keywords-alist)
(make-variable-buffer-local 'symbol-overlay-keywords-alist)

(defvar symbol-overlay-colors '("dodger blue"
				"hot pink"
				"orange"
				"orchid"
				"red"
				"salmon"
				"spring green"
				"turquoise")
  "Colors used for overlays' background.")

(defvar symbol-overlay-definition-function
  '(lambda (symbol) (concat "(?def[a-z-]* " symbol))
  "It must be an one-argument lambda function that returns a regexp.")
(make-variable-buffer-local 'symbol-overlay-definition-function)

(defun symbol-overlay-get-symbol (&optional str)
  "Get the symbol at point, if none, return nil.
If STR is non-nil, `regexp-quote' STR rather than the symbol."
  (let ((symbol (or str (thing-at-point 'symbol))))
    (if symbol (concat "\\_<" (regexp-quote symbol) "\\_>")
      (user-error "No symbol at point"))))

(defun symbol-overlay-put-overlay (symbol)
  "Put overlay to all occurrences of SYMBOL in the buffer.
The background color is randomly picked from `symbol-overlay-colors'."
  (let* ((case-fold-search nil)
	 (limit (length symbol-overlay-colors))
	 (index (random limit))
	 (indexes (mapcar 'cadr symbol-overlay-keywords-alist))
	 keyword color face overlay)
    (if (< (length symbol-overlay-keywords-alist) limit)
	(while (cl-find index indexes) (setq index (random limit)))
      (let ((oldest-keyword (car (last symbol-overlay-keywords-alist))))
	(symbol-overlay-remove (car oldest-keyword))
	(setq index (cadr oldest-keyword))))
    (setq keyword `(,symbol ,index)
	  color (elt symbol-overlay-colors index)
	  face `((foreground-color . "black")
		 (background-color . ,color)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward symbol nil t)
	(setq overlay (make-overlay (match-beginning 0) (match-end 0))
	      keyword (append keyword `(,overlay)))
	(overlay-put overlay 'face face)
	(overlay-put overlay 'keymap symbol-overlay-map)))
    (push keyword symbol-overlay-keywords-alist)
    color))

(defun symbol-overlay-count (symbol &optional color-msg)
  "Show the number of occurrences of SYMBOL.
If COLOR-MSG is non-nil, add the color used by current overlay in brackets."
  (let ((case-fold-search nil)
	(keyword (assoc symbol symbol-overlay-keywords-alist))
	overlay)
    (when keyword
      (setq overlay (car (overlays-at (point))))
      (when (stringp color-msg) (setq color-msg (concat " (" color-msg ")")))
      (message (concat (substring symbol 3 -3) ": %d/%d" color-msg)
	       (- (cl-position overlay keyword) 1)
	       (- (length keyword) 2)))))

;;;###autoload
(defun symbol-overlay-put ()
  "Toggle overlays of all occurrences of symbol at point."
  (interactive)
  (unless (minibufferp)
    (let ((symbol (symbol-overlay-get-symbol)))
      (if (assoc symbol symbol-overlay-keywords-alist)
	  (symbol-overlay-remove symbol)
	(when (looking-at-p "\\_>") (backward-char))
	(symbol-overlay-count symbol (symbol-overlay-put-overlay symbol))))))

;;;###autoload
(defun symbol-overlay-remove-all ()
  "Delete all highlighted symbols in the buffer."
  (interactive)
  (unless (minibufferp)
    (mapc 'symbol-overlay-remove (mapcar 'car symbol-overlay-keywords-alist))))

(defun symbol-overlay-remove (symbol)
  "Delete the highlighted SYMBOL."
  (let ((keyword (assoc symbol symbol-overlay-keywords-alist)))
    (setq symbol-overlay-keywords-alist
	  (delq keyword symbol-overlay-keywords-alist))
    (mapc 'delete-overlay (cddr keyword))))

;;;###autoload
(defun symbol-overlay-jump-next ()
  "Jump to the next location of symbol at point."
  (interactive)
  (symbol-overlay-jump-call 'symbol-overlay-basic-jump))

;;;###autoload
(defun symbol-overlay-jump-prev ()
  "Jump to the previous location of symbol at point."
  (interactive)
  (symbol-overlay-jump-call 'symbol-overlay-basic-jump -1))

;;;###autoload
(defun symbol-overlay-jump-to-definition ()
  "Jump to the definition of symbol at point.
The definition syntax should be defined in a lambda funtion stored in
`symbol-overlay-definition-function' that will return the definition's regexp
with the input symbol."
  (interactive)
  (symbol-overlay-jump-call
   '(lambda (symbol dir)
      (let ((p t) (pt (point)))
	(symbol-overlay-basic-jump symbol dir)
	(while (and p (not (save-excursion
			     (beginning-of-line)
			     (skip-chars-forward " \t")
			     (looking-at-p
			      (funcall symbol-overlay-definition-function
				       symbol)))))
	  (symbol-overlay-basic-jump symbol dir)
	  (when (= pt (point)) (setq p nil)))))))

(defun symbol-overlay-jump-call (jump-function &optional dir)
  "A general jumping process during which JUMP-FUNCTION is called to jump.
If optional argument DIR is non-nil, use it rather than the default value 1."
  (unless (minibufferp)
    (let ((symbol (symbol-overlay-get-symbol)))
      (setq mark-active nil)
      (funcall jump-function symbol (or dir 1))
      (push-mark nil t)
      (symbol-overlay-count symbol))))

(defun symbol-overlay-basic-jump (symbol dir)
  "Jump to SYMBOL's next location in the direction DIR.  Dir must be 1 or -1."
  (let* ((case-fold-search nil)
	 (bounds (bounds-of-thing-at-point 'symbol))
	 (offset (- (point) (if (> dir 0) (cdr bounds) (car bounds)))))
    (goto-char (- (point) offset))
    (let ((target (re-search-forward symbol nil t dir)))
      (unless target
	(goto-char (if (> dir 0) (point-min) (point-max)))
	(setq target (re-search-forward symbol nil nil dir)))
      (goto-char (+ target offset)))))

;;;###autoload
(defun symbol-overlay-query-replace ()
  "Command for query-replacing symbol at point."
  (interactive)
  (unless (minibufferp)
    (let* ((symbol (symbol-overlay-get-symbol))
	   (replacement (read-string "Replacement: "))
	   (defaults (cons symbol replacement)))
      (symbol-overlay-remove symbol)
      (beginning-of-thing 'symbol)
      (query-replace-regexp symbol replacement)
      (setq query-replace-defaults
	    (if (< emacs-major-version 25) `,defaults `(,defaults)))
      (symbol-overlay-put-overlay (symbol-overlay-get-symbol replacement)))))

(provide 'symbol-overlay)

;;; symbol-overlay.el ends here
