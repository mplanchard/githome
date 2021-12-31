# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

eval "$(ssh-agent)"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

. "$HOME/.cargo/env"

if [[ -f "/etc/profile.d/nix.sh" ]]; then
    . "/etc/profile.d/nix.sh"
fi

# Ensure nix applications get picked up in the gnome shell
export XDG_DATA_DIRS="$HOME/.nix-profile/share:$XDG_DATA_DIRS"

if [ "$DESKTOP_SESSION" = "i3" ]; then
    export "$(gnome-keyring-daemon -s)"
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi
