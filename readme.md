# Symbol Overlay

Highlighting symbols with overlays while providing a keymap for various operations about highlighted symbols.  It was originally inspired by the package `highlight-symbol`.  The fundamental difference is that in `symbol-overlay` every symbol is highlighted by the Emacs built-in function `overlay-put` rather than the `font-lock` mechanism used in `highlight-symbol`.

Advantages
---
### Fast
In `symbol-overlay`, `overlay-put` is much faster than the traditional highlighting method `font-lock` especially in a large buffer, or even a less-than-100-lines small buffer of major-mode with complicated keywords syntax, like haskell-mode.  Besides, all the overlays of each symbol are sequentially stored in an alist `symbol-overlay-keywords-alist`, from which the number of occurrences can be immediately obtained.  While in `highlight-symbol`, counting the number occurrences would call the function `how-many` twice, causing extra costs.
### Convenient
When highlighting symbols with overlays, **an auto-activated overlay-inside keymap** will enable you to call various useful commands with **a single keystroke**.
### Powerful
- Toggle overlays of all occurrences of symbol at point: `symbol-overlay-put`
- Remove all highlighted symbols in the buffer: `symbol-overlay-remove-all`
- Jump between locations of symbol at point: `symbol-overlay-jump-next` & `symbol-overlay-jump-prev`
- Jump to the definition of symbol at point: `symbol-overlay-jump-to-definition`
- Switch to the closest symbol highlighted nearby: `symbol-overlay-switch-forward` & `symbol-overlay-switch-backward`
- Query replace symbol at point: `symbol-overlay-query-replace`
- Rename symbol at point on all its occurrences: `symbol-overlay-rename`

Usage
---
To use `symbol-overlay` in your Emacs, you need only to bind three keys:

    (require 'symbol-overlay)
	(global-set-key (kbd "M-i") 'symbol-overlay-put)
	(global-set-key (kbd "M-u") 'symbol-overlay-switch-backward)
	(global-set-key (kbd "M-o") 'symbol-overlay-switch-forward)

Default key-bindings defined in `symbol-overlay-map`:

    "i" -> symbol-overlay-put
	"u" -> symbol-overlay-jump-prev
	"o" -> symbol-overlay-jump-next
	"k" -> symbol-overlay-remove-all
	"d" -> symbol-overlay-jump-to-definition
	"q" -> symbol-overlay-query-replace
	"n" -> symbol-overlay-rename

You can re-bind the commands to any keys you prefer by simply writing

    (define-key symbol-overlay-map (kbd "your-prefer-key") 'any-command)
