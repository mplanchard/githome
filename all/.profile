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

# if running bash
if [ -n "$BASH_VERSION" ]; then
	# include .bashrc if it exists
	if [ -f "$HOME/.bashrc" ]; then
		. "$HOME/.bashrc"
	fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin:$PATH"
fi

source "$HOME/.cargo/env"

if [[ -f "/etc/profile.d/nix.sh" ]]; then
	source "/etc/profile.d/nix.sh"
fi

if [ "$DESKTOP_SESSION" = "i3" ]; then
	export "$(gnome-keyring-daemon -s)"
fi