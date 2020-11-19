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
    if [ ! -f /etc/apt/sources.list.d/kelleyk-ubuntu-emacs-focal.list ]; then
        sudo add-apt-repository ppa:kelleyk/emacs
    fi

    PKGS=(
        build-essential     # pyenv
        ca-certificates     # email
        cmake
        curl                # pyenv
        direnv
        editorconfig
        emacs27             # emacs
        fd-find             # doom-emacs
        gconf2
        git                 # emacs, pyenv
        htop
        isync               # email
        jq                  # doom-emacs
        libbz2-dev          # pyenv
        libffi-dev          # pyenv
        liblzma-dev         # pyenv
        libncurses5-dev     # pyenv
        libncursesw5-dev    # pyenv
        libreadline-dev     # pyenv
        libsqlite3-dev      # pyenv
        libssl-dev          # pyenv
        libtool
        libtool-bin
        llvm                # pyenv
        maildir-utils       # email
        mu                  # email
        neovim
        pandoc
        postgresql
        postgresql-contrib
        python-openssl      # pyenv
        shellcheck
        sqlite3
        tk-dev              # pyenv
        wget                # pyenv
        xz-utils            # pyenv
        zlib1g-dev          # pyenv
    )

    sudo apt-get update

    sudo apt-get install -y "${PKGS[@]}"
    # See https://github.com/sharkdp/bat/issues/938
    sudo apt install -y -o Dpkg::Options::="--force-overwrite" bat ripgrep

    # we just use brew for golang on mac. Install manually here.
    if [[ "$(command -v go)" == "" ]]; then
        DL_PATH=/tmp/golang-cur.tar.gz
        rm -f "$DL_PATH"
        wget https://golang.org/dl/go1.15.3.linux-amd64.tar.gz -O "$DL_PATH"
        sudo tar -C /usr/local -xzf "$DL_PATH"
        rm -f "$DL_PATH"
    fi

    # Link certs into a common location for mac/linux
    if [[ ! -e "$HOME/.cert/cert.pem" ]]; then
        mkdir -p "$HOME/.cert"
        ln -s /etc/ssl/certs/ca-certificates.crt "$HOME/.cert/cert.pem"
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

    [[ ! -d /Applications/Alacritty.app ]] && brew cask install alacritty
    [[ ! -d /Applications/Docker.app ]] && brew cask install docker
    [[ ! -d /Applications/Dropbox.app ]] && brew cask install dropbox
    [[ ! -d "/Applications/Firefox Developer Edition.app" ]] && brew cask install homebrew/cask-versions/firefox-developer-edition
    [[ ! -d "/Applications/Firefox Nightly.app" ]] && brew cask install homebrew/cask-versions/firefox-nightly
    [[ ! -d /Applications/Firefox.app ]] && brew cask install firefox
    [[ ! -d "/Applications/Google Chrome.app" ]] && brew cask install google-chrome
    [[ ! -d "/Applications/GPG Keychain.app" ]] && brew cask install gpg-suite
    [[ ! -d /Applications/Slack.app ]] && brew cask install slack
    [[ ! -d /Applications/Virtualbox.app ]] && brew cask install virtualbox
    [[ ! $(which vagrant) ]] && brew cask install vagrant
    [[ ! -d "/Applications/Vagrant Manager.app" ]] && brew cask install vagrant-manager
    [[ ! -d "/Applications/Visual Studio Code.app" ]] && brew cask install visual-studio-code

    # Allow font installations
    brew tap homebrew/cask-fonts

    # for org-protocol capture
    brew cask install emacsclient
    brew cask install font-powerline-symbols
    brew cask install font-menlo-for-powerline
    brew cask install font-fira-mono-for-powerline
    brew cask install font-source-code-pro
    brew cask install font-ubuntu
    brew cask install font-ubuntu-mono

    unset HOMEBREW_NO_AUTO_UPDATE

    # Alacritty additions
    mkdir -p /usr/local/share/man/man1
    if [[ ! -d "$HOME/github/jwilm/alacritty" ]]; then
        git clone https://github.com/jwilm/alacritty.git $HOME/github/jwilm/alacritty
        # man page
        gzip -c $HOME/github/jwilm/alacritty/extra/alacritty.man \
            | tee /usr/local/share/man/man1/alacritty.1.gz > /dev/null
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
    mkdir -p ~/github/rust-analyzer/
    git clone \
        https://github.com/rust-analyzer/rust-analyzer.git \
        ~/github/rust-analyzer/rust-analyzer
    pushd ~/github/rust-analyzer/rust-analyzer
    cargo xtask install
    popd ~/github/rust-analyzer/rust-analyzer
fi
echo "Installing Rust components ..."
~/.cargo/bin/rustup component add clippy rustfmt rust-src

echo "Installing Rust utilities ..."
if [[ $(command -v watchexec) == "" ]]; then
    cargo install watchexec
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
    mkdir -p "$HOME/.vim/autoload/plug.vim"
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
~/.pyenv/bin/pyenv install --skip-existing 3.9.0

# Tmux (mac or linux)
echo "Adding tmux themes ..."
if [ ! -d "$HOME/github/jimeh/tmux-themepack" ]; then
    mkdir -p ~/github/jimeh/
    git clone https://github.com/jimeh/tmux-themepack.git ~/github/jimeh/tmux-themepack
fi

# Starship command prompt (mac or linux)
echo "Installing starship ..."
if [[ $(command -v starship) == "" ]];then
    curl -fsSL https://starship.rs/install.sh | bash
fi

# NVM (mac or linux)
if [[ ! -d "$HOME/.nvm" ]]; then
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.36.0/install.sh | bash
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    nvm install stable
    nvm alias default stable
fi

# Go language server
if [[ $(command -v gopls) == "" ]]; then
    GO111MODULE=on go get golang.org/x/tools/gopls@latest
fi

if [[ ! -e "$HOME/org" ]]; then
    echo "Symlink org notes to ~/org!"
fi

echo "Done!"
