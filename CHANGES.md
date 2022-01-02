0.4.0 (2022-01-02)
------------------

### Changed

- Increase contrast of color theme by choosing a darker shade of gray.
- Choose darker shades of colors for rainbow delimiters.


### Fixed

- Load custom-file.


0.3.0 (2021-10-29)
------------------

### Added

- Customize Rainbow Delimiters colors to show colorful parentheses.


0.2.0 (2021-02-21)
------------------

### Added

- Use spaces, not tabs, for indentation.


### Fixed

- Set `package-archives` before calling `package-initialize` to fix
  installation of new packages from MELPA after restarting Emacs.


0.1.0 (2020-12-16)
------------------

### Added

- Disable menu bar.
- Disable tool bar.
- Disable scroll bar.
- Use `wombat` theme.
- Enable `show-paren-mode` to highlight matching parentheses.
- Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
- Automatic installation of packages from MELPA.
- Write customizations to `~/.emacs.d/custom.el` instead of `~/.emacs`.
- Install SLIME.
- Install Paredit.
- Install Rainbow Delimiters.
- Set inferior Lisp program to `sbcl` in order to launch SLIME with it.
- Enable Paredit in Emacs Lisp mode, eval-expression minibuffer, IELM
  mode, Lisp mode, Lisp interaction mode, and SLIME REPL mode.
- Override the way SLIME handles <kbd>delete</kbd> key.
- Enable Rainbow Delimiters in Emacs Lisp mode, IELM mode, Lisp mode,
  Lisp interaction mode, and SLIME REPL mode.
