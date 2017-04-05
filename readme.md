# Symbol Overlay

Highlighting symbol while enabling you to jump from one occurrence to another or directly to the definition of that symbol in the buffer, with **A SINGLE KEYSTROKE**. It was originally inspired by the package `highlight-symbol`. The difference is that every symbol in `symbol-overlay` is highlighted by the emacs built-in function `overlay-put` rather than the `font-lock` mechanism used in `highlight-symbol`.

Advantages
---
- In `symbol-overlay`, `overlay-put` is much faster than the traditional highligting method `font-lock-fontify-buffer`, especially in a large buffer or even a less-than-100-lines small buffer of major-mode with complicated keywords syntax such as haskell-mode.
- More importantly, using `overlay-put` to highlight symbols has an extra benifit to enable **AN AUTO-ACTIVATED OVERLAY-INSIDE KEYMAP** for quick jump and other useful commands.
- You can also jump to a symbol's definition from any occurrence by using `so-jump-to-def`, as long as the syntax of the definition is specified in the buffer-local variable `so-def-function`.
- All the overlays of each symbol are stored sequentially in an alist `so-keywords-alist`. By simply getting the current overlay's index in the corresponding keyword-list as well as the length of it in the alist, the number of occurrences can be immediately obtained. While in `highlight-symbol`, this would call the function `how-many` twice, causing extra costs.

Usage
---
To use `symbol-overlay` in your Emacs, you need only to bind one key:

    (require 'symbol-overlay)
	(global-set-key (kbd "M-i") 'so-put)

A keymap `so-overlay-map` is already defined in the package:

"i" -> `so-put` : Toggle overlays of all occurrences of symbol at point.

"u" -> `so-jump-prev` : Jump to the previous location of symbol at point.

"o" -> `so-jump-next` : Jump to the next location of symbol at point.

"k" -> `so-remove-all` : Delete all highlighted symbols in the buffer.

"d" -> `so-jump-to-def` : Jump to the definition of symbol at point.

"q" -> `so-query-replace` : Command for query-replacing symbol at point.

You can customize the keymap by writing

    (define-key so-overlay-map (kbd "your-prefer-key") 'any-command)
