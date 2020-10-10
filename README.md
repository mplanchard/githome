# Home Directory

This repo is designed to be used as a bare git repo to track dotfiles, configs,
and other items in one's home directory.

A description of this process can be found [here](https://www.atlassian.com/git/tutorials/dotfiles). The main
exception is that the aliases and configs here assume `githome` as the
alias rather than `config`.

You can bootstrap by running:

``` sh
https://raw.githubusercontent.com/mplanchard/githome/master/scripts/githome.sh | bash
```

Note that if you have any existing files at any of the locations tracked
(almost always `~/.bashrc`), they will be moved to `~/.config-backup` and
replaced with the files in this repo.

Once cloned, you can use one of the scripts now in `~/scripts` to set up
your system.
