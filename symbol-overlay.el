;;; symbol-overlay.el --- Highlight symbols with keymap-enabled overlays

;; Copyright (C) 2017 wolray

;; Author: wolray <wolray@foxmail.com>
;; Version: 4.1
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

;; Highlighting symbols with overlays while providing a keymap for various
;; operations about highlighted symbols.  It was originally inspired by the
;; package `highlight-symbol'.  The fundamental difference is that in
;; `symbol-overlay' every symbol is highlighted by the Emacs built-in function
;; `overlay-put' rather than the `font-lock' mechanism used in
;; `highlight-symbol'.

;; Advantages

;; When highlighting symbols in a buffer of regular size and language,
;; `overlay-put' behaves as fast as the traditional highlighting method
;; `font-lock'.  However, for a buffer of major-mode with complicated keywords
;; syntax, like haskell-mode, `font-lock' is quite slow even the buffer is less
;; than 100 lines.  Besides, when counting the number of highlighted
;; occurrences, `highlight-symbol' will call the function `how-many' twice,
;; which could also result in an unpleasant delay in a large buffer.  Those
;; problems don't exist in `symbol-overlay'.

;; When putting overlays on symbols, an auto-activated overlay-inside keymap
;; will enable you to call various useful commands with a single keystroke.

;; Toggle all overlays of symbol at point: `symbol-overlay-put'
;; Jump between locations of symbol at point: `symbol-overlay-jump-next' &
;; `symbol-overlay-jump-prev'
;; Switch to the closest symbol highlighted nearby:
;; `symbol-overlay-switch-forward' & `symbol-overlay-switch-backward'
;; Minor mode for auto-highlighting symbol at point: `symbol-overlay-mode'
;; Remove all highlighted symbols in the buffer: `symbol-overlay-remove-all'
;; Copy symbol at point: `symbol-overlay-save-symbol'
;; Toggle overlays to be showed in buffer or only in scope:
;; `symbol-overlay-toggle-in-scope'
;; Jump back to the position before a recent jump: `symbol-overlay-echo-mark'
;; Jump to the definition of symbol at point: `symbol-overlay-jump-to-definition'
;; Isearch symbol at point literally, without `regexp-quote' the symbol:
;; `symbol-overlay-isearch-literally'
;; Query replace symbol at point: `symbol-overlay-query-replace'
;; Rename symbol at point on all its occurrences: `symbol-overlay-rename'

;; Usage

;; To use `symbol-overlay' in your Emacs, you need only to bind these keys:
;; (require 'symbol-overlay)
;; (global-set-key (kbd "M-i") 'symbol-overlay-put)
;; (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
;; (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
;; (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
;; (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)

;; Default key-bindings are defined in `symbol-overlay-map'.
;; You can re-bind the commands to any keys you prefer by simply writing
;; (define-key symbol-overlay-map (kbd "your-prefer-key") 'any-command)

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'seq)

(defgroup symbol-overlay nil
  "Highlight symbols with keymap-enabled overlays."
  :group 'convenience)

;;; Faces

(defface symbol-overlay-default-face
  '((t (:inherit highlight)))
  "Symbol Overlay default face"
  :group 'symbol-overlay)

(defface symbol-overlay-face-1
  '((t (:background "dodger blue" :foreground "black")))
  "Symbol Overlay default candidate 1"
  :group 'symbol-overlay)

(defface symbol-overlay-face-2
  '((t (:background "hot pink" :foreground "black")))
  "Symbol Overlay default candidate 2"
  :group 'symbol-overlay)

(defface symbol-overlay-face-3
  '((t (:background "yellow" :foreground "black")))
  "Symbol Overlay default candidate 3"
  :group 'symbol-overlay)

(defface symbol-overlay-face-4
  '((t (:background "orchid" :foreground "black")))
  "Symbol Overlay default candidate 4"
  :group 'symbol-overlay)

(defface symbol-overlay-face-5
  '((t (:background "red" :foreground "black")))
  "Symbol Overlay default candidate 5"
  :group 'symbol-overlay)

(defface symbol-overlay-face-6
  '((t (:background "salmon" :foreground "black")))
  "Symbol Overlay default candidate 6"
  :group 'symbol-overlay)

(defface symbol-overlay-face-7
  '((t (:background "spring green" :foreground "black")))
  "Symbol Overlay default candidate 7"
  :group 'symbol-overlay)

(defface symbol-overlay-face-8
  '((t (:background "turquoise" :foreground "black")))
  "Symbol Overlay default candidate 8"
  :group 'symbol-overlay)

;;; Options

(defcustom symbol-overlay-faces '(symbol-overlay-face-1
                                  symbol-overlay-face-2
                                  symbol-overlay-face-3
                                  symbol-overlay-face-4
                                  symbol-overlay-face-5
                                  symbol-overlay-face-6
                                  symbol-overlay-face-7
                                  symbol-overlay-face-8)
  "Faces used for overlays."
  :type '(repeat face)
  :group 'symbol-overlay)

(defcustom symbol-overlay-displayed-window t
  "See `symbol-overlay-maybe-put-temp'."
  :group 'symbol-overlay
  :type 'boolean)

(defcustom symbol-overlay-idle-time 0.5
  "Idle time after every command and before the temporary highlighting.

If nil highlighting is instant"
  :group 'symbol-overlay
  :type 'float)

(defcustom symbol-overlay-ignore-functions
  '((c-mode . symbol-overlay-ignore-function-c)
    (c++-mode . symbol-overlay-ignore-function-c++)
    (python-mode . symbol-overlay-ignore-function-python)
    (go-mode . symbol-overlay-ignore-function-go))
  "Functions to determine whether a symbol should be ignored.

This is an association list that maps a MAJOR-MODE symbol to a
function that determines whether a symbol should be ignored.
For instance, such a function could use a major mode's font-lock
definitions to prevent a language's keywords from getting highlighted."
  :group 'symbol-overlay
  :type '(repeat (cons (function :tag "Mode") function)))

;;; Internal

(defvar symbol-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'symbol-overlay-put)
    (define-key map (kbd "h") 'symbol-overlay-map-help)
    (define-key map (kbd "p") 'symbol-overlay-jump-prev)
    (define-key map (kbd "n") 'symbol-overlay-jump-next)
    (define-key map (kbd "<") 'symbol-overlay-jump-first)
    (define-key map (kbd ">") 'symbol-overlay-jump-last)
    (define-key map (kbd "w") 'symbol-overlay-save-symbol)
    (define-key map (kbd "t") 'symbol-overlay-toggle-in-scope)
    (define-key map (kbd "e") 'symbol-overlay-echo-mark)
    (define-key map (kbd "d") 'symbol-overlay-jump-to-definition)
    (define-key map (kbd "s") 'symbol-overlay-isearch-literally)
    (define-key map (kbd "q") 'symbol-overlay-query-replace)
    (define-key map (kbd "r") 'symbol-overlay-rename)
    map)
  "Keymap automatically activated inside overlays.
You can re-bind the commands to any keys you prefer.")

(defvar-local symbol-overlay-keywords-alist nil)
(put 'symbol-overlay-keywords-alist 'permanent-local t)

(defun symbol-overlay-map-help ()
  "Display the bindings in `symbol-overlay-map'."
  (interactive)
  (let ((buf (get-buffer-create "*Help*")))
    (with-help-window "*Help*"
      (with-current-buffer buf
        (insert (substitute-command-keys "\\{symbol-overlay-map}"))))))

;;;###autoload
(define-minor-mode symbol-overlay-mode
  "Minor mode for auto-highlighting symbol at point."
  nil " SO" (make-sparse-keymap)
  (if symbol-overlay-mode
      (progn
        (add-hook 'post-command-hook 'symbol-overlay-post-command nil t)
        (when symbol-overlay-idle-time
          (symbol-overlay-update-timer symbol-overlay-idle-time)))
    (remove-hook 'post-command-hook 'symbol-overlay-post-command t)
    (symbol-overlay-remove-temp)))

(defun symbol-overlay-get-list (dir &optional symbol exclude)
  "Get all highlighted overlays in the buffer.
If SYMBOL is non-nil, get the overlays that belong to it.
DIR is an integer.
If EXCLUDE is non-nil, get all overlays excluding those belong to SYMBOL."
  (let ((lists (progn (overlay-recenter (point)) (overlay-lists)))
        (func (if (> dir 0) 'cdr (if (< dir 0) 'car nil))))
    (seq-filter
     '(lambda (ov)
        (let ((value (overlay-get ov 'symbol)))
          (and value
               (or (not symbol)
                   (if (string= value symbol) (not exclude)
                     (and exclude (not (string= value ""))))))))
     (if func (funcall func lists)
       (append (car lists) (cdr lists))))))

(defun symbol-overlay-get-symbol (&optional string noerror)
  "Get the symbol at point.
If STRING is non-nil, `regexp-quote' STRING rather than the symbol.
If NOERROR is non-nil, just return nil when no symbol is found."
  (let ((symbol (or string (thing-at-point 'symbol))))
    (if symbol (concat "\\_<" (regexp-quote symbol) "\\_>")
      (unless noerror (user-error "No symbol at point")))))

(defun symbol-overlay-assoc (symbol)
  "Get SYMBOL's associated list in `symbol-overlay-keywords-alist'."
  (assoc symbol symbol-overlay-keywords-alist))

(defun symbol-overlay-maybe-remove (keyword)
  "Delete the KEYWORD list and all its overlays."
  (when keyword
    (mapc 'delete-overlay (symbol-overlay-get-list 0 (car keyword)))
    (setq symbol-overlay-keywords-alist
          (delq keyword symbol-overlay-keywords-alist))
    (cddr keyword)))

(defvar-local symbol-overlay-temp-symbol nil
  "Symbol for temporary highlighting.")

(defvar-local symbol-overlay-scope nil
  "If non-nil, force to narrow to scope before temporary highlighting.")

(defun symbol-overlay-narrow (scope &optional window)
  "Narrow to a specific region.
Region might be current scope or displayed window,
depending on SCOPE and WINDOW."
  (if scope
      (let ((pt (point))
            min max p)
        (save-excursion
          (save-restriction
            (narrow-to-defun)
            (setq min (point-min)
                  max (point-max)
                  p (or (/= pt (point)) (= pt (point-max))))))
        (save-excursion
          (and p (setq min (progn (backward-paragraph) (point))
                       max (progn (forward-paragraph) (point))))
          (narrow-to-region min max)))
    (when window
      (let ((lines (round (window-screen-lines)))
            (pt (point))
            beg)
        (save-excursion
          (forward-line (- lines))
          (setq beg (point))
          (goto-char pt)
          (forward-line lines)
          (narrow-to-region beg (point)))))))

(defun symbol-overlay-remove-temp ()
  "Delete all temporary overlays."
  (mapc 'delete-overlay (symbol-overlay-get-list 0 ""))
  (setq symbol-overlay-temp-symbol nil))

(defun symbol-overlay-maybe-put-temp ()
  "Highlight symbol at point when there are more than 2 occurrences.
This only effects symbols in the current displayed window if
`symbol-overlay-displayed-window' is non-nil."
  (when symbol-overlay-mode
    (let* ((case-fold-search nil)
           (symbol (symbol-overlay-get-symbol nil t))
           p)
      (when (and symbol
                 (not (symbol-overlay-assoc symbol))
                 (not (symbol-overlay-ignored-p symbol)))
        (symbol-overlay-remove-temp)
        (save-excursion
          (save-restriction
            (symbol-overlay-narrow symbol-overlay-scope
                                   symbol-overlay-displayed-window)
            (goto-char (point-min))
            (re-search-forward symbol nil t)
            (save-match-data
              (while (re-search-forward symbol nil t)
                (symbol-overlay-put-one symbol)
                (or p (setq p t))))
            (when p
              (symbol-overlay-put-one symbol)
              (setq symbol-overlay-temp-symbol symbol))))))))

(defun symbol-overlay-ignored-p (symbol)
  "Determine whether SYMBOL should be temporarily highlighted."
  (let ((f (cdr (assoc major-mode symbol-overlay-ignore-functions))))
    (when f
      (funcall f symbol))))

(defvar symbol-overlay-timer nil
  "Timer for temporary highlighting.")

(defun symbol-overlay-update-timer (value)
  "Update `symbol-overlay-timer' with new idle-time VALUE."
  (and symbol-overlay-timer (cancel-timer symbol-overlay-timer))
  (setq symbol-overlay-timer
        (and value (> value 0)
             (run-with-idle-timer value t 'symbol-overlay-maybe-put-temp))))

(defun symbol-overlay-post-command ()
  "Installed on `post-command-hook'."
  (unless (string= (symbol-overlay-get-symbol nil t) symbol-overlay-temp-symbol)
    (symbol-overlay-remove-temp)
    (unless symbol-overlay-idle-time
      (symbol-overlay-maybe-put-temp))))

(defun symbol-overlay-put-one (symbol &optional face)
  "Put overlay on current occurrence of SYMBOL after a match.
If FACE is non-nil, use it as the overlay’s face.
Otherwise apply `symbol-overlay-default-face'."
  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
    (if face (progn (overlay-put ov 'face face)
                    (overlay-put ov 'keymap symbol-overlay-map)
                    (overlay-put ov 'evaporate t)
                    (overlay-put ov 'symbol symbol))
      (overlay-put ov 'face 'symbol-overlay-default-face)
      (overlay-put ov 'symbol ""))))

(defun symbol-overlay-put-all (symbol scope &optional keyword)
  "Put overlays on all occurrences of SYMBOL in the buffer.
The face is randomly picked from `symbol-overlay-faces'.
If SCOPE is non-nil, put overlays only on occurrences in scope.
If KEYWORD is non-nil, remove it then use its color on new overlays."
  (let* ((case-fold-search nil)
         (limit (length symbol-overlay-faces))
         (face (or (symbol-overlay-maybe-remove keyword)
                   (elt symbol-overlay-faces (random limit))))
         (alist symbol-overlay-keywords-alist)
         (faces (mapcar 'cddr alist))
         (pt (point)))
    (if (< (length alist) limit)
        (while (seq-position faces face)
          (setq face (elt symbol-overlay-faces (random limit))))
      (setq face (symbol-overlay-maybe-remove (car (last alist)))))
    (and symbol-overlay-temp-symbol (symbol-overlay-remove-temp))
    (save-excursion
      (save-restriction
        (symbol-overlay-narrow scope)
        (goto-char (point-min))
        (while (re-search-forward symbol nil t)
          (symbol-overlay-put-one symbol face))))
    (setq keyword `(,symbol ,scope . ,face))
    (push keyword symbol-overlay-keywords-alist)
    keyword))

(defun symbol-overlay-maybe-count (keyword &optional show-color)
  "Show the number of KEYWORD's occurrences.
If SHOW-COLOR is non-nil, display the color used by current overlay."
  (when keyword
    (let* ((symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before))
           ;; Log to echo area but not *Messages*
           message-log-max)
      (message (concat (substring symbol 3 -3)
                       ": %d/%d"
                       (and (cadr keyword) " in scope")
                       (and show-color (format " (%s)" (cddr keyword))))
               (+ count 1)
               (+ count (length after))))))

(defun symbol-overlay-match-keyword-list (symbol keywords)
  "Return non-nil is SYMBOL is among KEYWORDS.
KEYWORDS is a list of strings.  SYMBOL is expected to include
leading \\_< and trailing \\_>, as the return value of
`symbol-overlay-get-symbol'."
  (cl-find (substring symbol 3 -3) keywords :test #'string=))

(defun symbol-overlay-refresh (beg end len)
  "Refresh overlays.  Installed on `after-change-functions'.
BEG, END and LEN are the beginning, end and length of changed text."
  (unless (or (minibufferp)
              (not (or symbol-overlay-keywords-alist
                       symbol-overlay-temp-symbol)))
    (let ((case-fold-search nil)
          (re "\\(\\sw\\|\\s_\\)+"))
      (save-excursion
        (save-match-data
          (goto-char end)
          (and (looking-at-p re)
               (setq end (re-search-forward "\\_>")))
          (goto-char beg)
          (and (not (looking-at-p "\\_<"))
               (looking-at-p (concat "\\(" re "\\|\\_>\\)"))
               (setq beg (re-search-backward "\\_<")))
          (mapc #'(lambda (ov)
                    (and (overlay-get ov 'symbol)
                         (delete-overlay ov)))
                (overlays-in beg end))
          (mapc #'(lambda (keyword)
                    (let ((symbol (car keyword)))
                      (goto-char beg)
                      (while (re-search-forward symbol end t)
                        (symbol-overlay-put-one symbol (cddr keyword)))))
                symbol-overlay-keywords-alist))))))

(add-hook 'after-change-functions 'symbol-overlay-refresh)

(defun symbol-overlay-after-revert ()
  "Restore overlays after the buffer was reverted."
  (save-restriction
    (widen)
    (symbol-overlay-refresh (point-min) (point-max) nil)))

(add-hook 'after-revert-hook 'symbol-overlay-after-revert)

;;; Language-Specific Ignore

(defvar c-font-lock-extra-types)
(defun symbol-overlay-ignore-function-c (symbol)
  "Determine whether SYMBOL should be ignored (C Language)."
  (symbol-overlay-match-keyword-list
   symbol
   (append c-font-lock-extra-types
           '("auto" "break" "case" "char" "const" "continue"
             "default" "do" "double" "else" "enum" "extern"
             "float" "for" "goto" "if" "inline" "int" "long"
             "register" "restrict" "return" "short" "signed"
             "sizeof" "static" "struct" "switch" "typedef"
             "union" "unsigned" "void" "volatile" "while"))))

(defvar c++-font-lock-extra-types)
(defun symbol-overlay-ignore-function-c++ (symbol)
  "Determine whether SYMBOL should be ignored (C++)."
  (symbol-overlay-match-keyword-list
   symbol
   (append c++-font-lock-extra-types
           '("alignas" "alignof" "asm" "auto" "bool" "break"
             "case" "catch" "char" "char16_t" "char32_t" "class"
             "const" "const_cast" "constexpr" "continue"
             "decltype" "default" "delete" "do" "double"
             "dynamic_cast" "else" "enum" "explicit" "export"
             "extern" "false" "final" "float" "for" "friend"
             "goto" "if" "inline" "int" "long" "mutable"
             "namespace" "new" "noexcept" "nullptr" "operator"
             "override" "private" "protected" "public" "register"
             "reinterpret_cast" "return" "short" "signed"
             "sizeof" "static" "static_assert" "static_cast"
             "struct" "switch" "template" "this" "thread_local"
             "throw" "true" "try" "typedef" "typeid" "typename"
             "union" "unsigned" "using" "virtual" "void"
             "volatile" "wchar_t" "while"))))

(defvar python-font-lock-keywords)
(defun symbol-overlay-ignore-function-python (symbol)
  "Determine whether SYMBOL should be ignored (Python)."
  (let* ((keyword-symbol (car python-font-lock-keywords))
         (keyword (if (stringp keyword-symbol)
                      keyword-symbol
                    (symbol-name keyword-symbol))))
    (string-match-p keyword symbol)))

(defvar go-builtins)
(defvar go-constants)
(defvar go-mode-keywords)
(defun symbol-overlay-ignore-function-go (symbol)
  "Determine whether SYMBOL should be ignored (Go)."
  ;; Remove \_< and \_> so we can string compare with keywords
  (or (symbol-overlay-match-keyword-list symbol go-builtins)
      (symbol-overlay-match-keyword-list symbol go-constants)
      (symbol-overlay-match-keyword-list symbol go-mode-keywords)))

;;; Commands

;;;###autoload
(defun symbol-overlay-put ()
  "Toggle all overlays of symbol at point."
  (interactive)
  (unless (minibufferp)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol)))
      (symbol-overlay-adjust-position)
      (if keyword
          (if (symbol-overlay-maybe-reput symbol keyword)
              (symbol-overlay-maybe-count keyword)
            (symbol-overlay-maybe-remove keyword)
            (symbol-overlay-maybe-put-temp))
        (symbol-overlay-maybe-count
         (symbol-overlay-put-all symbol symbol-overlay-scope)
         t)))))

(defun symbol-overlay-adjust-position ()
  "Backward one char if at the end of the symbol."
  (when (looking-at-p "\\_>") (backward-char)))

;;;###autoload
(defun symbol-overlay-count ()
  "Show count of symbol at point."
  (interactive)
  (unless (minibufferp)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol)))
      (symbol-overlay-adjust-position)
      (symbol-overlay-maybe-count keyword))))

;;;###autoload
(defun symbol-overlay-remove-all ()
  "Remove all highlighted symbols in the buffer.
When called interactively, then also reset
`symbol-overlay-keywords-alist'."
  (interactive)
  (unless (minibufferp)
    (mapc 'delete-overlay (symbol-overlay-get-list 0))
    (when (called-interactively-p 'any)
      (setq symbol-overlay-keywords-alist nil))))

(add-hook 'before-revert-hook 'symbol-overlay-remove-all)

;;;###autoload
(defun symbol-overlay-save-symbol ()
  "Copy symbol at point."
  (interactive)
  (unless (minibufferp)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (kill-ring-save (car bounds) (cdr bounds))
      (message "Current symbol saved"))))

;;;###autoload
(defun symbol-overlay-toggle-in-scope ()
  "Toggle overlays to be showed in buffer or only in scope."
  (interactive)
  (unless (minibufferp)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol)))
      (if keyword
          (let ((scope (not (cadr keyword))))
            (symbol-overlay-maybe-count
             (symbol-overlay-put-all symbol scope keyword))
            (setq symbol-overlay-scope scope))
        (setq symbol-overlay-scope (not symbol-overlay-scope))))))

(defun symbol-overlay-maybe-reput (symbol keyword)
  "Put overlays on SYMBOL that is not highlighted in scope.
KEYWORD provides the scope information."
  (when (and (cadr keyword)
             (not (seq-find #'(lambda (ov)
                                (string= (overlay-get ov 'symbol) symbol))
                            (overlays-at
                             (car (bounds-of-thing-at-point 'symbol))))))
    (symbol-overlay-put-all symbol t keyword)))

;;;###autoload
(defun symbol-overlay-echo-mark ()
  "Jump back to the mark."
  (interactive)
  (let* ((pt (mark))
         (symbol (symbol-overlay-get-symbol))
         (keyword (symbol-overlay-assoc symbol)))
    (and pt (goto-char pt))
    (symbol-overlay-maybe-reput symbol keyword)))

(defun symbol-overlay-jump-call (jump-function dir)
  "A general jumping process during which JUMP-FUNCTION is called to jump.
DIR must be non-zero."
  (unless (minibufferp)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol)))
      (push-mark nil t)
      (funcall jump-function symbol dir)
      (when keyword
        (symbol-overlay-maybe-reput symbol keyword)
        (symbol-overlay-maybe-count keyword)))))

(defun symbol-overlay-basic-jump (symbol dir)
  "Jump to SYMBOL's next location in the direction DIR.
DIR must be non-zero."
  (let* ((case-fold-search nil)
         (bounds (bounds-of-thing-at-point 'symbol))
         (offset (- (point) (if (> dir 0) (cdr bounds) (car bounds))))
         target)
    (goto-char (- (point) offset))
    (setq target (re-search-forward symbol nil t dir))
    (unless target
      (goto-char (if (> dir 0) (point-min) (point-max)))
      (setq target (re-search-forward symbol nil nil dir)))
    (goto-char (+ target offset))))

;;;###autoload
(defun symbol-overlay-jump-next ()
  "Jump to the next location of symbol at point."
  (interactive)
  (symbol-overlay-adjust-position)
  (symbol-overlay-jump-call 'symbol-overlay-basic-jump 1))

;;;###autoload
(defun symbol-overlay-jump-prev ()
  "Jump to the previous location of symbol at point."
  (interactive)
  (symbol-overlay-adjust-position)
  (symbol-overlay-jump-call 'symbol-overlay-basic-jump -1))

;;;###autoload
(defun symbol-overlay-jump-first ()
  "Jump to the first location."
  (interactive)
  (symbol-overlay-adjust-position)
  (let* ((symbol (symbol-overlay-get-symbol))
         (before (symbol-overlay-get-list -1 symbol))
         (count (length before)))
    (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count))))

;;;###autoload
(defun symbol-overlay-jump-last ()
  "Jump to the last location."
  (interactive)
  (symbol-overlay-adjust-position)
  (let* ((symbol (symbol-overlay-get-symbol))
         (after (symbol-overlay-get-list 1 symbol))
         (count (length after)))
    (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count 1))))

(defvar-local symbol-overlay-definition-function
  '(lambda (symbol) (concat "(?def[a-z-]* " symbol))
  "An one-argument function that returns a regexp.")

;;;###autoload
(defun symbol-overlay-jump-to-definition ()
  "Jump to the definition of symbol at point.
The definition syntax should be defined in a function stored in
`symbol-overlay-definition-function' that returns the definition's regexp
with the input symbol."
  (interactive)
  (symbol-overlay-jump-call
   '(lambda (symbol dir)
      (let ((pt (point)) p)
        (symbol-overlay-basic-jump symbol dir)
        (while (not (or p (save-excursion
                            (beginning-of-line)
                            (skip-chars-forward " \t")
                            (looking-at-p
                             (funcall symbol-overlay-definition-function
                                      symbol)))))
          (symbol-overlay-basic-jump symbol dir)
          (and (= pt (point)) (setq p t)))))
   1))

(defun symbol-overlay-switch-symbol (dir)
  "Switch to the closest symbol highlighted nearby, in the direction DIR.
DIR must be 1 or -1."
  (unless (minibufferp)
    (let* ((symbol (symbol-overlay-get-symbol nil t))
           (list (symbol-overlay-get-list dir symbol t)))
      (or list
          (user-error (concat "No more "
                              (if (> dir 0) "forward" "backward")
                              " symbols")))
      (push-mark nil t)
      (goto-char (overlay-start (car list)))
      (symbol-overlay-maybe-count
       (symbol-overlay-assoc (symbol-overlay-get-symbol))))))

;;;###autoload
(defun symbol-overlay-switch-forward ()
  "Switch forward to another symbol."
  (interactive)
  (symbol-overlay-switch-symbol 1))

;;;###autoload
(defun symbol-overlay-switch-backward ()
  "Switch backward to another symbol."
  (interactive)
  (symbol-overlay-switch-symbol -1))

;;;###autoload
(defun symbol-overlay-isearch-literally ()
  "Isearch symbol at point literally, without `regexp-quote' the symbol."
  (interactive)
  (unless (minibufferp)
    (let ((symbol (symbol-overlay-get-symbol)))
      (beginning-of-thing 'symbol)
      (isearch-forward nil t)
      (isearch-yank-string (substring symbol 3 -3)))))

;;;###autoload
(defun symbol-overlay-query-replace ()
  "Query replace symbol at point."
  (interactive)
  (unless (minibufferp)
    (let* ((case-fold-search nil)
           (symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (scope (cadr keyword))
           txt defaults new)
      (and scope (user-error "Query-replace invalid in scope"))
      (beginning-of-thing 'symbol)
      (push-mark nil t)
      (setq txt (read-string "Replacement: " (substring symbol 3 -3))
            new (symbol-overlay-get-symbol txt)
            defaults (cons symbol txt))
      (unless (string= new symbol)
        (symbol-overlay-maybe-remove (symbol-overlay-assoc new))
        (setq keyword (symbol-overlay-put-all new scope keyword))
        (query-replace-regexp symbol txt)
        (setq query-replace-defaults
              (if (< emacs-major-version 25) `,defaults `(,defaults))))
      (when (string= new (symbol-overlay-get-symbol nil t))
        (beginning-of-thing 'symbol)
        (symbol-overlay-maybe-count keyword)))))

;;;###autoload
(defun symbol-overlay-rename ()
  "Rename symbol at point on all its occurrences."
  (interactive)
  (unless (minibufferp)
    (let* ((case-fold-search nil)
           (symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (scope (if keyword (cadr keyword) symbol-overlay-scope))
           txt new)
      (beginning-of-thing 'symbol)
      (push-mark nil t)
      (setq txt (read-string (concat "Rename" (and scope " in scope") ": ")
                             (substring symbol 3 -3))
            new (symbol-overlay-get-symbol txt))
      (unless (string= new symbol)
        (symbol-overlay-maybe-remove (symbol-overlay-assoc new))
        (save-excursion
          (save-restriction
            (symbol-overlay-narrow scope)
            (goto-char (point-min))
            (let ((inhibit-modification-hooks t))
              (while (re-search-forward symbol nil t) (replace-match txt t)))))
        (when keyword
          (setq keyword (symbol-overlay-put-all new scope keyword))))
      (when (string= new (symbol-overlay-get-symbol nil t))
        (symbol-overlay-maybe-count keyword)))))

;;; _
(provide 'symbol-overlay)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; symbol-overlay.el ends here
