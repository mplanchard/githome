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
	# Repositories
	echo "Checking for emacs PPA ..."
	if [ ! -f /etc/apt/sources.list.d/kelleyk-ubuntu-emacs-focal.list ] &&
		[ ! -f /etc/apt/sources.list.d/kelleyk-ubuntu-emacs-groovy.list ]; then
		sudo add-apt-repository ppa:kelleyk/emacs
	fi

	echo "Checking for hashicorp PPA..."
	if [ ! -f /etc/apt/sources.list.d/archive_uri-https_apt_releases_hashicorp_com-groovy.list ]; then
		curl -fsSL https://apt.releases.hashicorp.com/gpg | sudo apt-key add -
		sudo apt-add-repository "deb [arch=$(dpkg --print-architecture)] https://apt.releases.hashicorp.com $(lsb_release -cs) main"
	fi

	PKGS=(
		build-essential # pyenv
		ca-certificates # email
		cmake
		curl # pyenv
		direnv
		editorconfig
		emacs27 # emacs
		fd-find # doom-emacs
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
		lldb
		llvm # pyenv
		neovim
		nodejs
		npm
		nscd # nameservice caching daemon, used by guix
		pandoc
		postgresql
		postgresql-contrib
		python3-openssl # pyenv
		rust-lldb
		shellcheck
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

	SNAPS=(
		maildir-utils # mu & mu4e
	)

	sudo apt-get update

	sudo apt-get install -y "${PKGS[@]}"
	# See https://github.com/sharkdp/bat/issues/938
	sudo apt install -y -o Dpkg::Options::="--force-overwrite" bat ripgrep

	sudo snap install "${SNAPS[@]}"

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
		ln -s /etc/ssl/certs/ca-certificates.crt "$HOME/.cert/cert.pem"
	fi

	# Install guix pacakge manager
	if [[ "$(command -v guix)" == "" ]]; then
		mkdir -p "$HOME/Downloads"
		GUIX_INSTALL_PATH="$HOME/Downloads/guix-install.sh"

		# Ensure we've got the GNU public key
		wget 'https://sv.gnu.org/people/viewgpg.php?user_id=15145' -qO - | sudo -i gpg --import -

		curl https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh -o "$GUIX_INSTALL_PATH"

		GUIX_MD5="b0355947de8ef1ec38c0c9dfb4c2afbe"
		if [[ "$GUIX_MD5" != $(md5sum "$GUIX_INSTALL_PATH" | awk '{print $1}') ]]; then
			echo "guix md5sum has changed. please verify it looks okay, then update this script"
			exit 1
		else
			sudo sh "$GUIX_INSTALL_PATH"
		fi
		guix pull

		guix install glibc-utf8-locales gs-fonts font-dejavu font-gnu-freefont
	fi

	if [[ "$(command -v nix-env)" == "" ]]; then
		sh <(curl -L https://nixos.org/nix/install) --daemon
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
