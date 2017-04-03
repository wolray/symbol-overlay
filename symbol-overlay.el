;;; symbol-overlay.el --- Putting overlays on symbol and fast jumping in between.

;; Highlighting symbol and enabling you to jump from one occurrence to another
;; or even to the definition in the buffer, in any language, with a single key.
;; It was originally inspired by the package 'highlight-symbol. The difference or
;; improvement is that every symbol in 'symbol-overlay is highlighted by the emacs
;; built-in function `overlay-put' rather than the font-lock mechanism used in
;; 'highlight-symbol. Besides, when counting the occurrences of the symbol,
;; 'symbol-overlay needs  only to simply obtain the current occurrence's index in
;; the keywords' association list as well as the length of it where all the
;; overlays are stored in order. While in 'highlight-symbol, this would call the
;; function `how-many' twice, causing a perceivable delay in a large buffer.
;;
;; `overlay-put' is much faster than `font-lock-fontify-buffer', especially in a
;; large buffer, or even a less-than-100-lines small buffer of major-mode with
;; complicated keywords syntax such as haskell-mode.
;;
;; You can also jump to a symbol's definition from any occurrence using
;; `so-jump-to-def' , as long as the syntax of the definition is specified in the
;; buffer-local variable `so-def-function'.
;;
;; More importantly, using `overlay-put' to highlight-symbol has an extra
;; advantage to enable a inside-overlay keymap for quick jump as well as other
;; related operations by just making a single key strike.

;;; Code:

(require 'thingatpt)
(eval-when-compile (require 'cl))

(defvar so-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'so-put)
    (define-key map (kbd "o") 'so-jump-next)
    (define-key map (kbd "p") 'so-remove-all)
    (define-key map (kbd "q") 'so-query-replace)
    (define-key map (kbd "u") 'so-jump-prev)
    (define-key map (kbd "y") 'so-jump-to-def)
    map)
  "keymap automatically activated inside overlays.
You can re-bind them to any keys you prefer.")

(defvar so-keywords)
(make-variable-buffer-local 'so-keywords)

(defvar so-colors '("dodger blue"
		    "hot pink"
		    "orange"
		    "orchid"
		    "red"
		    "salmon"
		    "spring green"
		    "turquoise")
  "Colors use for overlays' background")

(defvar so-def-function
  '(lambda (symbol) (concat "(?def[a-z-]* " symbol))
  "It must be a one-argument lambda function and returns a regexp")
(make-variable-buffer-local 'so-def-function)

(defun so-get-s (&optional str)
  "Get the symbol at point, if none, return nil. If STR is non-nil, regexp-quote
STR rather than the symbol."
  (let ((symbol (or str (thing-at-point 'symbol))))
    (when symbol (concat "\\_<" (regexp-quote symbol) "\\_>"))))

(defun so-get-s-error ()
  (user-error "No symbol at point."))

(defun so-put-s (symbol)
  "Put overlay to all occurrences of SYMBOL in the buffer, using a random
background color defined in `so-colors'."
  (let* ((case-fold-search nil)
	 (limit (length so-colors))
	 (index (random limit))
	 (indexes (mapcar 'cadr so-keywords))
	 color face keyword overlay)
    (when (>= (length so-keywords) limit) (user-error "No more color"))
    (while (cl-find index indexes)
      (setq index (random limit)))
    (setq color (elt so-colors index)
	  face `((foreground-color . "black")
		 (background-color . ,color))
	  keyword `(,symbol ,index))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward symbol nil t)
	(setq overlay (make-overlay (match-beginning 0) (match-end 0))
	      keyword (append keyword `(,overlay)))
	(overlay-put overlay 'face face)
	(overlay-put overlay 'keymap so-overlay-keymap)))
    (push keyword so-keywords)
    color))

(defun so-count-s (symbol &optional color-msg)
  "Show the number of current occurrence of SYMBOL, if COLOR-MSG is non-nil,
add the color used by current overlay in brackets."
  (let ((case-fold-search nil)
	(keyword (assoc symbol so-keywords))
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
      (unless symbol (so-get-s-error))
      (if (assoc symbol so-keywords) (so-remove-s symbol)
	(when (looking-at-p "\\_>") (backward-char))
	(so-count-s symbol (so-put-s symbol))))))

;;;###autoload
(defun so-remove-all ()
  "Delete all highlighted symbols in the buffer."
  (interactive)
  (unless (minibufferp)
    (mapc 'so-remove-s (mapcar 'car so-keywords))))

(defun so-remove-s (symbol)
  "Delete the highlighted SYMBOL."
  (let ((keyword (assoc symbol so-keywords)))
    (setq so-keywords (delq keyword so-keywords))
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
      (unless symbol (so-get-s-error))
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
  (interactive)
  (unless (minibufferp)
    (let ((symbol (so-get-s)))
      (unless symbol (so-get-s-error))
      (if (assoc symbol so-keywords) (so-query-replace-s symbol)
	(message "Symbol not highlighted")))))

(defun so-query-replace-s (symbol)
  "Query replace SYMBOL with replacement inputed in a prompt."
  (let ((replacement (read-string "Replacement: ")))
    (so-remove-s symbol)
    (beginning-of-thing 'symbol)
    (query-replace-regexp symbol replacement)
    (setq query-replace-defaults `(,(cons symbol replacement)))
    (so-put-s (so-get-s replacement))))

(provide 'symbol-overlay)

;;; symbol-overlay.el ends here
