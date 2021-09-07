# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

if [ -e "$PYENV_ROOT/bin/pyenv" ]; then
    eval "$(pyenv init --path)"
fi

eval "$(ssh-agent)"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

. "$HOME/.cargo/env"

if [[ -f "/etc/profile.d/nix.sh" ]]; then
    . "/etc/profile.d/nix.sh"
fi

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

# if [ -d "$HOME/.guix-profile" ]; then
#     export GUIX_PROFILE="/home/matthew/.guix-profile"
#     . "$GUIX_PROFILE/etc/profile"
# fi

# # Load the "main" guix profile, with most of the standard stuff in it
# if [ -d "$HOME/.guix-extra-profiles/main" ]; then
#     profile="$HOME/.guix-extra-profiles/main/main"
#     GUIX_PROFILE="$profile"
#     . "$GUIX_PROFILE/etc/profile"

#     export GUIX_LOCPATH="$profile/lib/locale"
#     export SSL_CERT_DIR="$profile/etc/ssl/certs"
#     export SSL_CERT_FILE="$profile/etc/ssl/certs/ca-certificates.crt"
#     export GIT_SSL_CAINFO="$SSL_CERT_FILE"

#     unset profile
# fi
