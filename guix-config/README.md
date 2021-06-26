# Guix Main Profie Config

This needs to be managed separately, because `~/.config/guix` itself is a
symlink, and if the stow target of a package is a symlink, stow will assume it
shouldn't be managing it.

Usage:

```sh
stow -t ~/.config/guix
```
