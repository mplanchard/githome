#!/usr/bin/env bash
set -euo pipefail

LINUX="linux"
MAC="mac"

if uname -a | grep -q "Darwin"; then
	ENV="$MAC"
else
	ENV="$LINUX"
fi

# Install packages (linux)
if [[ "$ENV" == "$LINUX" ]]; then

	# Prereqs
	sudo apt-get install build-essential curl git wget

	# Repositories
	echo "Checking for emacs PPA ..."
	if [ ! -f /etc/apt/sources.list.d/kelleyk-ubuntu-emacs-focal.list ] &&
		[ ! -f /etc/apt/sources.list.d/kelleyk-ubuntu-emacs-groovy.list ]; then
		sudo add-apt-repository ppa:kelleyk/emacs
	fi

	echo "Checking for hashicorp PPA..."
	if [ ! -f /etc/apt/sources.list.d/archive_uri-https_apt_releases_hashicorp_com-focal.list ] &&
		[ ! -f /etc/apt/sources.list.d/archive_uri-https_apt_releases_hashicorp_com-groovy.list ]; then
		curl -fsSL https://apt.releases.hashicorp.com/gpg | sudo apt-key add -
		sudo apt-add-repository "deb [arch=$(dpkg --print-architecture)] https://apt.releases.hashicorp.com $(lsb_release -cs) main"
	fi

	echo "Checking for signal PPA"
	if [ ! -f /etc/apt/sources.list.d/signal-xenial.list ]; then
		wget -O- https://updates.signal.org/desktop/apt/keys.asc |
			gpg --dearmor >signal-desktop-keyring.gpg
		sudo mv signal-desktop-keyring.gpg /usr/share/keyrings/
		echo 'deb [arch=amd64 signed-by=/usr/share/keyrings/signal-desktop-keyring.gpg] https://updates.signal.org/desktop/apt xenial main' |
			sudo tee -a /etc/apt/sources.list.d/signal-xenial.list
	fi

	echo "Checking for 1Password PPA..."
	if [ ! -f "/etc/apt/sources.list.d/1password-beta.list" ]; then
		curl -sS https://downloads.1password.com/linux/keys/1password.asc | sudo apt-key add -
		echo 'deb [arch=amd64] https://downloads.1password.com/linux/debian/amd64 beta main' |
			sudo tee /etc/apt/sources.list.d/1password-beta.list
		sudo mkdir -p /etc/debsig/policies/AC2D62742012EA22/
		curl -sS https://downloads.1password.com/linux/debian/debsig/1password.pol |
			sudo tee /etc/debsig/policies/AC2D62742012EA22/1password.pol
		sudo mkdir -p /usr/share/debsig/keyrings/AC2D62742012EA22
		curl -sS https://downloads.1password.com/linux/keys/1password.asc |
			sudo gpg --dearmor --output /usr/share/debsig/keyrings/AC2D62742012EA22/debsig.gpg
	fi

	PKGS=(
		1password
		build-essential # pyenv
		ca-certificates # email
		cmake
		curl # pyenv
		direnv
		editorconfig
		emacs27               # emacs
		emacs-common-non-dfsg # emacs docs
		fd-find               # doom-emacs
		gconf2
		git # emacs, pyenv
		htop
		i3
		isync            # email
		jq               # doom-emacs
		libbz2-dev       # pyenv
		libffi-dev       # pyenv
		liblzma-dev      # pyenv
		libncurses5-dev  # pyenv
		libncursesw5-dev # pyenv
		libreadline-dev  # pyenv
		libsqlite3-dev   # pyenv
		libssl-dev       # pyenv
		libtool
		libtool-bin
		lldb-11
		llvm          # pyenv
		maildir-utils # mu
		mu4e
		neovim
		nodejs
		npm
		nscd # nameservice caching daemon, used by guix
		pandoc
		postgresql
		postgresql-contrib
		python3-lldb-11
		python3-openssl # pyenv
		rust-lldb
		shellcheck
		signal-desktop # from the signal repository
		sqlite3
		terraform # from the hashicorp repository
		texlive-latex-base
		texlive-latex-extra
		texlive-fonts-recommended
		texlive-latex-recommended
		texlive-latex-recommended-doc
		tidy       # org-mode html export
		tk-dev     # pyenv
		wget       # pyenv
		xz-utils   # pyenv
		zlib1g-dev # pyenv
	)

	CLASSIC_SNAPS=(
		slack
	)

	SNAPS=(
		discord
		zulip
	)

	sudo apt-get update

	sudo apt-get install -y "${PKGS[@]}"
	# See https://github.com/sharkdp/bat/issues/938
	sudo apt install -y -o Dpkg::Options::="--force-overwrite" bat ripgrep

	# sudo snap install "${SNAPS[@]}"
	# sudo snap install --classic "${CLASSIC_SNAPS[@]}"

	# we just use brew for golang on mac. Install manually here.
	if [[ "$(command -v go)" == "" ]]; then
		DL_PATH=/tmp/golang-cur.tar.gz
		rm -f "$DL_PATH"
		wget https://golang.org/dl/go1.15.3.linux-amd64.tar.gz -O "$DL_PATH"
		sudo tar -C /usr/local -xzf "$DL_PATH"
		rm -f "$DL_PATH"
		export PATH="$PATH:/usr/local/go/bin"
	fi

	# Link certs into a common location for mac/linux
	if [[ ! -e "$HOME/.cert/cert.pem" ]]; then
		mkdir -p "$HOME/.cert"
		ln -s /etc/ssl/certs/ca-certificates.crt "$HOME/.cert/cert.pem"
	fi

	if [[ "$(command -v nix-env)" == "" ]]; then
		sh <(curl -L https://nixos.org/nix/install) --daemon --no-modify-profile
	fi

	source "/etc/profile.d/nix.sh"

	nix-env -i niv lorri

	if [[ "$(command -v dropbox)" == "" ]]; then
		wget https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2020.03.04_amd64.deb \
			-O ~/Downloads/install-dropbox.deb
		sudo apt-get install ~/Downloads/install-dropbox.deb
	fi

	if [[ "$(command -v zoom)" == "" ]]; then
		# Add public key to keyring
		wget https://zoom.us/linux/download/pubkey -O - | gpg --import

		# downoad zoom installer
		ZOOM_DL="$HOME/Downloads/zoom-install.deb"
		wget https://zoom.us/client/latest/zoom_amd64.deb -O "$ZOOM_DL"

		# Check signature
		ZOOM_FINGERPRINT="3960 60CA DD8A 7522 0BFC  B369 B903 BF18 61A7 C71D"
		if gpg --verify "$ZOOM_DL" 2>&1 | grep -q "$ZOOM_FINGERPRINT"; then
			sudo apt-get install -y "$ZOOM_DL"
		else
			echo "Failed to verify zoom signature!"
			exit 1
		fi
	fi

else
	export HOMEBREW_NO_AUTO_UPDATE=1

	brew update

	# Just in case a mac update breaks this
	sudo chown -R $(whoami) /usr/local/*

	brew tap d12frosted/emacs-plus

	# Note: to make life easier, pyenv and nvm are installed using their
	# standard install scripts rather than through brew, so that they'll
	# wind up installed in the same locations on mac and linux, which is
	# important for us to be able to set environments easily using the same
	# commands on either system
	BREW_PKGS=" \
        aspell \
        bash-completion \
        bat \
        cmake \
        coreutils \
        direnv \
        editorconfig \
        emacs-plus \
        exa \
        fd \
        fzf \
        git \
        go \
        gpg \
        graphviz \
        htop \
        isync \
        mas \
        mu \
        neovim \
        node \
        openssl \
        pandoc \
        postgresql \
        ripgrep \
        shellcheck \
        tmux \
        tokei \
        vim \
        wget"

	for PKG in $BREW_PKGS; do
		brew install $PKG || brew upgrade $PKG
	done

	if [[ ! $(xcode-select --version) ]]; then
		xcode-select --install
	fi

	if [[ ! -e /Applications/Emacs.app ]]; then
		ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
	fi

	[[ ! -d /Applications/Alacritty.app ]] && brew install --cask alacritty
	[[ ! -d /Applications/Docker.app ]] && brew install --cask docker
	[[ ! -d /Applications/Dropbox.app ]] && brew install --cask dropbox
	[[ ! -d "/Applications/Firefox Developer Edition.app" ]] && brew install --cask homebrew/cask-versions/firefox-developer-edition
	[[ ! -d "/Applications/Firefox Nightly.app" ]] && brew install --cask homebrew/cask-versions/firefox-nightly
	[[ ! -d /Applications/Firefox.app ]] && brew install --cask firefox
	[[ ! -d "/Applications/Google Chrome.app" ]] && brew install --cask google-chrome
	[[ ! -d "/Applications/GPG Keychain.app" ]] && brew install --cask gpg-suite
	[[ ! -d /Applications/Slack.app ]] && brew install --cask slack
	[[ ! -d /Applications/Virtualbox.app ]] && brew install --cask virtualbox
	[[ ! $(which vagrant) ]] && brew install --cask vagrant
	[[ ! -d "/Applications/Vagrant Manager.app" ]] && brew install --cask vagrant-manager
	[[ ! -d "/Applications/Visual Studio Code.app" ]] && brew install --cask visual-studio-code

	# Allow font installations
	brew tap homebrew/cask-fonts

	# for org-protocol capture
	brew install --cask emacsclient
	brew install --cask font-powerline-symbols
	brew install --cask font-menlo-for-powerline
	brew install --cask font-fira-mono-for-powerline
	brew install --cask font-source-code-pro
	brew install --cask font-ubuntu
	brew install --cask font-ubuntu-mono

	unset HOMEBREW_NO_AUTO_UPDATE

	# Alacritty additions
	mkdir -p /usr/local/share/man/man1
	if [[ ! -d "$HOME/github/jwilm/alacritty" ]]; then
		git clone https://github.com/jwilm/alacritty.git $HOME/github/jwilm/alacritty
		# man page
		gzip -c $HOME/github/jwilm/alacritty/extra/alacritty.man |
			tee /usr/local/share/man/man1/alacritty.1.gz >/dev/null
		# terminfo
		tic -xe alacritty,alacritty-direct $HOME/github/jwilm/extra/alacritty.info
	fi

	# Install Magnet from the app store
	echo "checking magnet install"
	MAGNET_ID=$(mas search magnet | /usr/local/bin/rg "Magnet +\(" | awk '{print $1}')
	mas install "$MAGNET_ID"

	# Install 1password from the app store
	echo "checking 1password install"
	ONEPW_ID=$(mas search 1Password | /usr/local/bin/rg "1Password 7 - Password Manager +\(" | awk '{print $1}')
	mas install "$ONEPW_ID"

	# Install 1password from the app store
	echo "checking Evernote install"
	EVERNOTE_ID=$(mas search Evernote | /usr/local/bin/rg "^ +\d+ +Evernote +\(" | awk '{print $1}')
	mas install "$EVERNOTE_ID"

	# Install amphetamine
	echo "checking amphetamine install"
	AMPHETAMINE_ID=$(mas search Amphetamine | /usr/local/bin/rg "Amphetamine +\(" | awk '{print $1}')
	mas install "$AMPHETAMINE_ID"

	# Link certs into a common location for mac/linux
	if [[ ! -e "$HOME/.cert/cert.pem" ]]; then
		mkdir -p "$HOME/.cert"
		ln -s /usr/local/etc/openssl/cert.pem "$HOME/.cert/cert.pem"
	fi

fi

# Make directories for mail
mkdir -p ~/.mail/gmail
mkdir -p ~/.mail/work

# Ensure we've got our standard GH directory
mkdir -p ~/github/

# Rust (mac or linux)
echo "Installing Rust ..."
if [[ $(command -v rustc) == "" ]]; then
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	export PATH="$PATH:$HOME/.cargo/bin"
	mkdir -p ~/github/rust-analyzer/
	if [[ ! -d ~/github/rust-analyzer/rust-analyzer ]]; then
		git clone \
			https://github.com/rust-analyzer/rust-analyzer.git \
			~/github/rust-analyzer/rust-analyzer
	fi
	pushd ~/github/rust-analyzer/rust-analyzer
	if [[ $(command -v code) == "" ]]; then
		# Do not perform VSCode-specific install steps
		cargo xtask install --server
	else
		cargo xtask install
	fi
	popd
fi
echo "Installing Rust components ..."
~/.cargo/bin/rustup component add clippy rustfmt rust-src

echo "Installing Rust utilities ..."
if [[ $(command -v watchexec) == "" ]]; then
	cargo install watchexec
fi
if [[ $(command -v exa) == "" ]]; then
	cargo install exa
fi

# Doom Emacs (mac or linux)
echo "Installing Doom emacs ..."
if [ ! -f "$HOME/.emacs.d/README.md" ]; then
	git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
	~/.emacs.d/bin/doom install
fi

# Vim (mac or linux)
echo "Configuring vim ..."
mkdir -p ~/.vim/backup/
mkdir -p ~/.vim/swap/
if [[ ! -f "$HOME/.vim/autoload/plug.vim" ]]; then
	mkdir -p "$HOME/.vim/autoload"
	curl -fLo "$HOME/.vim/autoload/plug.vim" --create-dirs \
		"https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
fi

# Python (mac or linux)
echo "Installing python versions ..."
if [[ $(command -v pyenv) == "" ]]; then
	curl https://pyenv.run | bash
fi

# Ensure pyenv is on the latest version
echo "Ensure pyenv is up to date ..."
pushd "$HOME/.pyenv"
git pull
popd

# Note: these should be updated along with the `pyenv global` call in .bashrc
echo "Installing python versions ..."
~/.pyenv/bin/pyenv install --skip-existing 3.8.6
~/.pyenv/bin/pyenv install --skip-existing 3.7.9
~/.pyenv/bin/pyenv install --skip-existing 3.6.12
~/.pyenv/bin/pyenv install --skip-existing 3.9.1

# Tmux (mac or linux)
echo "Adding tmux themes ..."
if [ ! -d "$HOME/github/jimeh/tmux-themepack" ]; then
	mkdir -p ~/github/jimeh/
	git clone https://github.com/jimeh/tmux-themepack.git ~/github/jimeh/tmux-themepack
fi

# Starship command prompt (mac or linux)
echo "Installing starship ..."
if [[ $(command -v starship) == "" ]]; then
	curl -fsSL https://starship.rs/install.sh | bash
fi

# NVM (mac or linux)
if [[ ! -d "$HOME/.nvm" ]]; then
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.36.0/install.sh | bash
	export NVM_DIR="$HOME/.nvm"
	[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
	nvm install stable
	nvm alias default stable
fi

# Go language server
if [[ $(command -v gopls) == "" ]]; then
	GO111MODULE=on go get golang.org/x/tools/gopls@latest
fi

# shfmt
if [[ $(command -v shfmt) == "" ]]; then
	GO111MODULE=on go get mvdan.cc/sh/v3/cmd/shfmt
fi

if [[ ! -e "$HOME/org" ]]; then
	echo "Symlink org notes to ~/org!"
fi

# Configure Git
echo "Configuring git"
git config --global commit.gpgsign true

echo "Done!"
