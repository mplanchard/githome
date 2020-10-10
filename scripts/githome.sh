#!/usr/bin/env bash
set -euo pipefail

shopt -s expand_aliases

alias githome='git --git-dir $HOME/.dotfiles --work-tree=$HOME'

if [ ! -f ~/.gitignore/ ]; then
    echo ".dotfiles" > ~/.gitignore
fi

if [ ! -d ~/.dotfiles/ ]; then
    git clone --bare git@github.com:mplanchard/githome ~/.dotfiles
    mkdir -p ~/.config-backup && \
        githome checkout 2>&1 \
        | egrep "\s+\." \
        | awk '{print $1}' \
        | xargs -I{} mv {} ~/.config-backup/{}
    githome checkout
    githome config --local status.showUntrackedFiles no
fi
