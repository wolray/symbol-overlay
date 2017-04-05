;;; symbol-overlay.el --- Highlighting symbols with a powerful keymap

;; Copyright (C) 2017 wolray

;; Author: wolray <wolray@foxmail.com>
;; URL: https://github.com/wolray/symbol-overlay/
;; Keywords: faces, matching

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
;; KEYSTROKE. It was originally inspired by the package `highlight-symbol'. The
;; difference is that every symbol in `symbol-overlay' is highlighted by the
;; Emacs built-in function `overlay-put' rather than the `font-lock' mechanism
;; used in `highlight-symbol'.

;; Advantages
;; 1. In `symbol-overlay', `overlay-put' is much faster than the traditional
;; highligting method `font-lock-fontify-buffer', especially in a large buffer
;; or even a less-than-100-lines small buffer of major-mode with complicated
;; keywords syntax such as haskell-mode.
;; 2. You can directly jump to a symbol's definition from any occurrence by
;; using `so-jump-to-def', as long as the syntax of the definition is specified
;; in the buffer-local variable `so-def-function'.
;; 3. More importantly, using `overlay-put' to highlight-symbol has an extra
;; benifit to enable AN AUTO-ACTIVATED OVERLAY-INSIDE KEYMAP for quick jump and
;; other operations.
;; 4. All the overlays of each symbol are stored sequentially in an alist
;; `so-keywords-alist'. By simply getting the current overlay's index in the
;; alist as well as the length of it, the number of occurrences can be
;; immediately obtained. While in `highlight-symbol', this would call the
;; function `how-many' twice, causing extra costs.

;; Usage
;; To use `symbol-overlay' in your Emacs, you need only to bind one key:
;; (require 'symbol-overlay)
;; (global-set-key (kbd "M-i") 'so-put)

;;; Code:

(require 'thingatpt)
(eval-when-compile (require 'cl))

(defvar so-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'so-put)
    (define-key map (kbd "u") 'so-jump-prev)
    (define-key map (kbd "o") 'so-jump-next)
    (define-key map (kbd "k") 'so-remove-all)
    (define-key map (kbd "d") 'so-jump-to-def)
    (define-key map (kbd "q") 'so-query-replace)
    map)
  "Keymap automatically activated inside overlays.
You can re-bind the commands to any keys you prefer.")

(defvar so-keywords-alist)
(make-variable-buffer-local 'so-keywords-alist)

(defvar so-colors '("dodger blue"
		    "hot pink"
		    "orange"
		    "orchid"
		    "red"
		    "salmon"
		    "spring green"
		    "turquoise")
  "Colors used for overlays' background.")

(defvar so-def-function
  '(lambda (symbol) (concat "(?def[a-z-]* " symbol))
  "It must be an one-argument lambda function that returns a regexp.")
(make-variable-buffer-local 'so-def-function)

(defun so-get-s (&optional str)
  "Get the symbol at point, if none, return nil. If STR is non-nil,
`regexp-quote' STR rather than the symbol."
  (let ((symbol (or str (thing-at-point 'symbol))))
    (if symbol (concat "\\_<" (regexp-quote symbol) "\\_>")
      (user-error "No symbol at point"))))

(defun so-put-s (symbol)
  "Put overlay to all occurrences of SYMBOL in the buffer, using a random
background color from `so-colors'."
  (let* ((case-fold-search nil)
	 (limit (length so-colors))
	 (index (random limit))
	 (indexes (mapcar 'cadr so-keywords-alist))
	 keyword color face overlay)
    (if (< (length so-keywords-alist) limit)
	(while (cl-find index indexes) (setq index (random limit)))
      (let ((oldest-keyword (car (last so-keywords-alist))))
	(so-remove-s (car oldest-keyword))
	(setq index (cadr oldest-keyword))))
    (setq keyword `(,symbol ,index)
	  color (elt so-colors index)
	  face `((foreground-color . "black")
		 (background-color . ,color)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward symbol nil t)
	(setq overlay (make-overlay (match-beginning 0) (match-end 0))
	      keyword (append keyword `(,overlay)))
	(overlay-put overlay 'face face)
	(overlay-put overlay 'keymap so-overlay-map)))
    (push keyword so-keywords-alist)
    color))

(defun so-count-s (symbol &optional color-msg)
  "Show the number of occurrences of SYMBOL. If COLOR-MSG is non-nil, add the
color used by current overlay in brackets."
  (let ((case-fold-search nil)
	(keyword (assoc symbol so-keywords-alist))
        overlay)
    (when keyword
      (setq overlay (car (overlays-at (point))))
      (when (stringp color-msg) (setq color-msg (concat " (" color-msg ")")))
      (message (concat (substring symbol 3 -3) ": %d/%d" color-msg)
	       (- (cl-position overlay keyword) 1)
	       (- (length keyword) 2)))))

;;;###autoload
(defun so-put ()
  "Toggle overlays of all occurrences of symbol at point."
  (interactive)
  (unless (minibufferp)
    (let ((symbol (so-get-s)))
      (if (assoc symbol so-keywords-alist) (so-remove-s symbol)
	(when (looking-at-p "\\_>") (backward-char))
	(so-count-s symbol (so-put-s symbol))))))

;;;###autoload
(defun so-remove-all ()
  "Delete all highlighted symbols in the buffer."
  (interactive)
  (unless (minibufferp)
    (mapc 'so-remove-s (mapcar 'car so-keywords-alist))))

(defun so-remove-s (symbol)
  "Delete the highlighted SYMBOL."
  (let ((keyword (assoc symbol so-keywords-alist)))
    (setq so-keywords-alist (delq keyword so-keywords-alist))
    (mapc 'delete-overlay (cddr keyword))))

;;;###autoload
(defun so-jump-next ()
  "Jump to the next location of symbol at point."
  (interactive)
  (so-jump-call 'so-jump-s))

;;;###autoload
(defun so-jump-prev ()
  "Jump to the previous location of symbol at point."
  (interactive)
  (so-jump-call 'so-jump-s t))

;;;###autoload
(defun so-jump-to-def ()
  "Jump to the definition of symbol at point. The definition syntax should be
defined in a lambda funtion stored in `so-def-function' that will return the
definition's regexp with the input symbol."
  (interactive)
  (so-jump-call
   '(lambda (symbol dir)
      (let ((p t) (pt (point)))
	(so-jump-s symbol dir)
	(while (and p (not (save-excursion
			     (beginning-of-line)
			     (skip-chars-forward " \t")
			     (looking-at-p
			      (funcall so-def-function symbol)))))
	  (so-jump-s symbol dir)
	  (when (= pt (point)) (setq p nil)))))))

(defun so-jump-call (jump-function &optional back)
  "A general jumping process during which JUMP-FUNCTION is called to jump to a
nearby occurrence or the definition of the symbol. If BACK is non-nil, reverse
the jumping direction."
  (unless (minibufferp)
    (let ((symbol (so-get-s)))
      (setq mark-active nil)
      (funcall jump-function symbol (if back -1 1))
      (push-mark nil t)
      (so-count-s symbol))))

(defun so-jump-s (symbol dir)
  "Jump to SYMBOL's next location in the direction DIR."
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
(defun so-query-replace ()
  "Command for query-replacing current symbol."
  (interactive)
  (unless (minibufferp)
    (let ((symbol (so-get-s)))
      (if (assoc symbol so-keywords-alist) (so-query-replace-s symbol)
	(message "Symbol not highlighted")))))

(defun so-query-replace-s (symbol)
  "Query replace SYMBOL with replacement from a prompt."
  (let* ((replacement (read-string "Replacement: "))
	 (defaults (cons symbol replacement)))
    (so-remove-s symbol)
    (beginning-of-thing 'symbol)
    (query-replace-regexp symbol replacement)
    (setq query-replace-defaults
	  (if (>= emacs-major-version 25) `(,defaults) `,defaults))
    (so-put-s (so-get-s replacement))))

(provide 'symbol-overlay)

;;; symbol-overlay.el ends here
