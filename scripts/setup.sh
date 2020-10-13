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
        curl                # pyenv
        direnv
        editorconfig
        emacs27             # emacs
        fd-find             # doom-emacs
        git                 # emacs, pyenv
        htop
        jq                  # doom-emacs
        libbz2-dev          # pyenv
        libffi-dev          # pyenv
        liblzma-dev         # pyenv
        libncurses5-dev     # pyenv
        libncursesw5-dev    # pyenv
        libreadline-dev     # pyenv
        libsqlite3-dev      # pyenv
        libssl-dev          # pyenv
        llvm                # pyenv
        neovim
        python-openssl      # pyenv
        tk-dev              # pyenv
        wget                # pyenv
        xz-utils            # pyenv
        zlib1g-dev          # pyenv
    )

    sudo apt-get update

    sudo apt-get install -y "${PKGS[@]}"
    # See https://github.com/sharkdp/bat/issues/938
    sudo apt install -y -o Dpkg::Options::="--force-overwrite" bat ripgrep
fi

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
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# Python (mac or linux)
echo "Installing python versions ..."
if [[ $(command -v pyenv) == "" ]]; then
    curl https://pyenv.run | bash
fi
~/.pyenv/bin/pyenv install --skip-existing 3.8.6 
~/.pyenv/bin/pyenv install --skip-existing 3.7.9
~/.pyenv/bin/pyenv install --skip-existing 3.6.12

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
if [[ $(command -v nvm) == "" ]]; then
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.36.0/install.sh | bash
fi
