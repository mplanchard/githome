# Matthew's Dotfiles

This repo contains dotfiles and other config files for a variety of systems.
It's intended to be deployed with [GNU Stow], which is a symlink manager. Each
top-level directory is a stow "package," containing symlinks for a given system
or environment. The `all` package are dotfiles I install on every system,
`linux` on linux, `gnome` in gnome, etc.

To use it to manage dotfiles, you'll run something like:

```sh
$ sudo apt-get install stow
$ mkdir -p ~/source/gh/mplanchard/
$ git clone https://github.com/mplanchard/githome ~/source/gh/mplanchard/githome
# Install dotfiles common to all platforms
$ stow --dir ~/source/gh/mplanchard --target ~ all
# Install linux dotfiles
$ stow --dir ~/source/gh/mplanchard --target ~ linux
```

There's also a [setup script] that aims to get everything fully operation from a
reasonable base state. Currently it's focused on Ubuntu and Mac, although the
latter hasn't been tested in quite some time and is probably out of date and/or
broken.

**Do not clone this and use it blindly.** The setup script will currently
overwrite any existing files that are managed by this dotfiles repository,
because that's what I want when I'm setting up a new machine. It might not be
what you want if you're just exploring this repo, though!

[gnu stow]: https://www.gnu.org/software/stow/
[setup script]: ./scripts/setup.sh
