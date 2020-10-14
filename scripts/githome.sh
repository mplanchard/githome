#!/usr/bin/env bash

# Note we can't use pipefail here b/c we're relying on the potentially
# failed first checkout to back stuff up.
set -euo 

shopt -s expand_aliases

alias githome='git --git-dir $HOME/.dotfiles --work-tree=$HOME'

if [ ! -f ~/.gitignore/ ]; then
    echo ".dotfiles" > ~/.gitignore
fi

if [ ! -d ~/.dotfiles/ ]; then
    git clone --bare git@github.com:mplanchard/githome ~/.dotfiles
    githome config --local status.showUntrackedFiles no
    # Try to checkout. If we get error output, it's probably because
    # we can't overwrite stuff that's here. Parse the output and
    # move the files, then try again. Note that this will _still_
    # fail if the files being replaced are symlinks
    mkdir -p ~/.config-backup && \
        githome checkout 2>&1 \
        | egrep "\s+\." \
        | awk '{print $1}' \
        | xargs -I{} 'mkdir -p "~/.config-backup/$(dirname {})" && mv {} ~/.config-backup/{}'
    githome checkout
fi
