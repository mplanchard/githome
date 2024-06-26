#!/usr/bin/env bash
set -euo pipefail

# TODO: add emacs28 install
# TODO: add mu install

UPGRADE=
for i in "$@"; do
    case $i in
        -u | --upgrade)
            UPGRADE=1
            shift # past argument=value
            ;;
        *)
            # unknown option
            ;;
    esac
done

LINUX="linux"
MAC="mac"

if uname -a | grep -q "Darwin"; then
    ENV="$MAC"
else
    ENV="$LINUX"
fi

SOURCE="$HOME/s"
GH="$SOURCE/gh"
DOWNLOADS="$HOME/Downloads"
BINS="$HOME/bin"
DOTFILES="$GH/mplanchard/dotfiles"

# Make some common directories
mkdir -p "$SOURCE"
mkdir -p "$GH"
mkdir -p "$DOWNLOADS"
mkdir -p "$BINS"
mkdir -p "$DOTFILES"

# Instlal stow packages, overwriting conflicts.
install_stow_overwrite() {
    STOW_DIR=$1
    TARGET_DIR=$2
    PACKAGE=$3
    # First delete any conflicting files
    pushd "$TARGET_DIR"
    stow --dir "$STOW_DIR" --target "$TARGET_DIR" --simulate "$PACKAGE" 2>&1 |
        grep "existing target" |
        awk -F ': ' '{print $2}' |
        xargs rm -rf
    popd
    # Then install the stowed package
    stow --dir "$STOW_DIR" --target "$TARGET_DIR" "$PACKAGE"
}

checkout_dotfiles() {
    if [[ ! -e "$DOTFILES/.git" ]]; then
        # use https since we probably haven't set up an SSH key yet
        git clone https://github.com/mplanchard/githome "$TARGET"
    fi
}

# Install packages (linux)
if [[ "$ENV" == "$LINUX" ]]; then

    # Prereqs
    sudo apt-get install build-essential curl git wget

    # Repositories
    echo "Checking for 1Password PPA..."
    if [ ! -f "/etc/apt/sources.list.d/1password.list" ]; then
        curl -sS https://downloads.1password.com/linux/keys/1password.asc |
            sudo gpg --dearmor --output /usr/share/keyrings/1password-archive-keyring.gpg

        echo 'deb [arch=amd64 signed-by=/usr/share/keyrings/1password-archive-keyring.gpg] https://downloads.1password.com/linux/debian/amd64 stable main' |
            sudo tee /etc/apt/sources.list.d/1password.list

        sudo mkdir -p /etc/debsig/policies/AC2D62742012EA22/
        curl -sS https://downloads.1password.com/linux/debian/debsig/1password.pol |
            sudo tee /etc/debsig/policies/AC2D62742012EA22/1password.pol

        sudo mkdir -p /usr/share/debsig/keyrings/AC2D62742012EA22
        curl -sS https://downloads.1password.com/linux/keys/1password.asc |
            sudo gpg --dearmor --output /usr/share/debsig/keyrings/AC2D62742012EA22/debsig.gpg
    fi

    PKGS=(
        1password
        apt-file
        ca-certificates # email
        editorconfig
        fonts-firacode
        fonts-font-awesome
        fonts-hack
        fonts-mononoki
        gconf2
        gir1.2-clutter-1.0
        gir1.2-gtop-2.0
        gir1.2-nm-1.0
        gnome-sushi # file preview
        gnome-system-monitor
        gnome-tweaks
        gstreamer1.0-plugins-bad
        gstreamer1.0-plugins-ugly
        i3
        libbz2-dev             # pyenv
        libc6-i386             # steam
        libffi-dev             # pyenv
        libgl1:i386            # steam
        libgl1-mesa-dri:i386   # steam
        libgtk-3-dev           # PopOS tiling shortcuts
        libimage-exiftool-perl # music tag reading
        liblzma-dev            # pyenv
        libncurses5-dev        # pyenv
        libncursesw5-dev       # pyenv
        libreadline-dev        # pyenv
        libsqlite3-dev         # pyenv
        libssl-dev             # pyenv
        libtool
        libtool-bin
        make
        pavucontrol
        playerctl
        pm-utils
        rust-lldb
        texlive-fonts-recommended
        texlive-latex-base
        texlive-latex-extra
        texlive-latex-recommended
        texlive-latex-recommended-doc
        tk-dev # pyenv
        ubuntu-gnome-desktop
        wmctrl
        xautolock
        x11-utils
        xz-utils   # pyenv
        zlib1g-dev # pyenv
    )

    CLASSIC_SNAPS=(
        slack
    )

    SNAPS=(
        discord
        spotify
        vlc
        zulip
    )

    sudo apt-get update

    sudo apt-get install -y "${PKGS[@]}"
    # See https://github.com/sharkdp/bat/issues/938
    sudo apt install -y -o Dpkg::Options::="--force-overwrite" bat ripgrep

    if [[ "$UPGRADE" ]]; then
        sudo apt-get upgrade -y
    fi

    echo "Installing dotfiles..."
    checkout_dotfiles
    install_stow_overwrite "$DOTFILES" "$HOME" all
    install_stow_overwrite "$DOTFILES" "$HOME" linux
    install_stow_overwrite "$DOTFILES" "$HOME" gnome
    echo "Dotfiles successfully installed!"

    # snap is so dummmb
    for PKG in "${SNAPS[@]}"; do
        if ! snap list | awk '{print $1}' | grep -q "$PKG"; then
            sudo snap install "$PKG"
        fi
    done
    for PKG in "${CLASSIC_SNAPS[@]}"; do
        if ! snap list | awk '{print $1}' | grep -q "$PKG"; then
            sudo snap install --classic "$PKG"
        fi
    done

    if [[ "$UPGRADE" ]]; then
        sudo snap refresh
    fi

    echo "Checking golang install..."
    if [[ "$(command -v go)" == "" || "$UPGRADE" ]]; then
        echo "Installing golang..."
        GO_DL="$DOWNLOADS/golang-cur.tar.gz"
        rm -f "$GO_DL"
        wget https://golang.org/dl/go1.15.3.linux-amd64.tar.gz -O "$GO_DL"
        sudo tar -C /usr/local -xzf "$GO_DL"
        rm -f "$GO_DL"
        export PATH="$PATH:/usr/local/go/bin"
        echo "golang successfully installed"
    fi

    echo "Checking ca-certificates symlink..."
    # Link certs into a common location for mac/linux
    if [[ ! -e "$HOME/.cert/cert.pem" ]]; then
        echo "Symlinking ca-certificates"
        mkdir -p "$HOME/.cert"
        ln -s /etc/ssl/certs/ca-certificates.crt "$HOME/.cert/cert.pem"
        echo "ca-certificates symlinked successfully"
    fi

    echo "Checking nix install..."
    if [[ "$(command -v nix-env)" == "" ]]; then
        echo "Installilng nix"
        sh <(curl -L https://nixos.org/nix/install) --daemon --no-modify-profile
        echo "Nix successfully installed"
    fi

    source "/etc/profile.d/nix.sh"

    if [[ "$(command -v niv)" == "" || "$UPGRADE" ]]; then
        nix-env -i niv
    fi

    if [[ "$(command -v lorri)" == "" || "$UPGRADE" ]]; then
        nix-env -i lorri
    fi

    if [[ "$(command -v i3-status-rs)" == "" || "$UPGRADE" ]]; then
        nix-env -i i3status-rust
    fi

    echo "Checking kmonad install"
    if [[ "$(command -v kmonad)" == "" || "$UPGRADE" ]]; then
        CHECKOUT="$GH/kmonad/kmonad"
        if [[ -d "$CHECKOUT" ]]; then
            pushd "$CHECKOUT"
            git reset --hard HEAD
            git pull
        else
            mkdir -p "$CHECKOUT"
            git clone https://github.com/kmonad/kmonad "$CHECKOUT"
            pushd "$CHECKOUT"
        fi
        nix-build nix
        ln -s "$(readlink result)/bin/kmonad" "$HOME/bin/kmonad"
        popd
    fi

    echo "Checking Dropbox install..."
    if [[ "$(command -v dropbox)" == "" || "$UPGRADE" ]]; then
        echo "Installing Dropbox..."
        DROPBOX_DL="$DOWNLOADS/install-dropbox.deb"
        wget https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2020.03.04_amd64.deb \
            -O "$DROPBOX_DL"
        sudo apt-get install "$DROPBOX_DL"
        rm -f "$DROPBOX_DL"
        echo "Dropbox successfully installed"
    fi

    echo "Checking Zoom install..."
    if [[ "$(command -v zoom)" == "" || "$UPGRADE" ]]; then
        echo "Installing Zoom..."

        # Add public key to keyring
        if ! gpg --list-keys | grep -q "Zoom Video"; then
            wget https://zoom.us/linux/download/pubkey -O - | gpg --import
        fi

        # downoad zoom installer
        ZOOM_DL="$HOME/Downloads/zoom-install.deb"
        wget https://zoom.us/client/latest/zoom_amd64.deb -O "$ZOOM_DL"

        # Check signature
        ZOOM_FINGERPRINT="3960 60CA DD8A 7522 0BFC  B369 B903 BF18 61A7 C71D"
        if gpg --verify "$ZOOM_DL" 2>&1 | grep -q "$ZOOM_FINGERPRINT"; then
            sudo apt-get install -y "$ZOOM_DL"
            rm -f "$ZOOM_DL"
        else
            echo "Failed to verify zoom signature!"
            exit 1
        fi
        echo "Zoom successfully installed"
    fi

    echo "Checking Steam install..."
    if [[ "$(command -v steam)" == "" || "$UPGRADE" ]]; then
        STEAM_DL="$HOME/Downloads/steam-install.deb"
        wget https://cdn.akamai.steamstatic.com/client/installer/steam.deb -O "$STEAM_DL"
        sudo apt-get install -y ~/Downloads/steam-install.deb
        rm -f "$STEAM_DL"
        echo "Steam successfully installed"
    fi

else

    export HOMEBREW_NO_AUTO_UPDATE=1

    brew update

    # Just in case a mac update breaks this
    sudo chown -R "$(whoami)" /usr/local/*

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
        eza \
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
        brew install "$PKG" || brew upgrade "$PKG"
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

    echo "Installing dotfiles..."
    checkout_dotfiles
    install_stow_overwrite "$DOTFILES" "$HOME" all
    echo "Dotfiles successfully installed!"

    # Alacritty additions
    mkdir -p /usr/local/share/man/man1
    if [[ ! -d "$GH/jwilm/alacritty" ]]; then
        git clone https://github.com/jwilm/alacritty.git "$GH/jwilm/alacritty"
        # man page
        gzip -c "$GH/jwilm/alacritty/extra/alacritty.man" |
            tee /usr/local/share/man/man1/alacritty.1.gz >/dev/null
        # terminfo
        tic -xe alacritty,alacritty-direct "$GH/jwilm/extra/alacritty.info"
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

# Rust (mac or linux)
echo "Checking Rust Install..."
if [[ $(command -v rustc) == "" ]]; then
    echo "Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    export PATH="$PATH:$HOME/.cargo/bin"
    echo "Successfully installed Rust"
fi

echo "Checking Rust components ..."
~/.cargo/bin/rustup component add clippy rustfmt rust-src

if [[ "$UPGRADE" ]]; then
    echo "Upgrading Rust and its components..."
    rustup update
    echo "Rust successfully updated"
fi

echo "Checking rust-analyzer install..."
RA_REPO="$GH/rust-analyzer/rust-analyzer"
if [[ ! -d "$RA_REPO" || "$UPGRADE" ]]; then
    echo "Installing rust-analyzer..."
    if [[ ! -d "$RA_REPO" ]]; then
        git clone https://github.com/rust-analyzer/rust-analyzer.git "$RA_REPO"
    fi
    pushd "$RA_REPO"

    if [[ "$UPGRADE" ]]; then
        git fetch && git checkout master && git reset --hard origin/master
    fi

    if [[ $(command -v code) == "" ]]; then
        # Do not perform VSCode-specific install steps
        cargo xtask install --server
    else
        cargo xtask install
    fi

    popd
    echo "rust-analyzer successfully installed"
fi

echo "Installing Rust utilities ..."
if [[ $(command -v watchexec) == "" || "$UPGRADE" ]]; then
    cargo install watchexec-cli
fi
if [[ $(command -v eza) == "" || "$UPGRADE" ]]; then
    cargo install eza
fi
if [[ $(command -v btm) == "" || "$UPGRADE" ]]; then
    cargo install bottom
fi
if [[ $(command -v delta) == "" || "$UPGRADE" ]]; then
    cargo install git-delta
fi

# Doom Emacs (mac or linux)
echo "Checking Doom emacs ..."
if [[ ! -f "$HOME/.emacs.d/README.md" ]]; then
    echo "Installing Doom emacs..."
    git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom install
    echo "Doom emacs successfully installed"
fi

if [[ "$UPGRADE" ]]; then
    echo "Upgrading doom emacs..."
    doom upgrade
    doom build
    doom sync
    doom env
    echo "Doom emacs successfully upgraded"
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
echo "Installing pyenv..."
if [[ $(command -v pyenv) == "" ]]; then
    curl https://pyenv.run | bash
fi

# Ensure pyenv is on the latest version
if [[ "$UPGRADE" ]]; then
    echo "Updating pyenv..."
    pushd "$HOME/.pyenv"
    git pull
    popd
    echo "Pyenv is up to date"
fi

# Note: these should be updated along with the `pyenv global` call in .bashrc
echo "Installing python versions ..."
~/.pyenv/bin/pyenv install --skip-existing 3.8.6
~/.pyenv/bin/pyenv install --skip-existing 3.7.9
~/.pyenv/bin/pyenv install --skip-existing 3.6.12
~/.pyenv/bin/pyenv install --skip-existing 3.9.1

# Tmux (mac or linux)
echo "Checking tmux themes ..."
TMUX_THEME_REPO="$GH/jimeh/tmux-themepack"
if [[ ! -d "$TMUX_THEME_REPO" || "$UPGRADE" ]]; then
    echo "Installing tmux themes ..."
    mkdir -p "$TMUX_THEME_REPO"
    git clone https://github.com/jimeh/tmux-themepack.git "$TMUX_THEME_REPO"
    echo "Tmux themes successfully installed"
fi

# Starship command prompt (mac or linux)
echo "Chekcing starship ..."
if [[ $(command -v starship) == "" || "$UPGRADE" ]]; then
    echo "Installing starship..."
    sh -c "$(curl -fsSL https://starship.rs/install.sh)"
    echo "Starship successfully installed"
fi

# NVM (mac or linux)
echo "Checking nvm...."
if [[ ! -d "$HOME/.nvm" || "$UPGRADE" ]]; then
    echo "Installing nvm..."
    if [[ ! -d "$HOME/.nvm" ]]; then
        curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.36.0/install.sh | bash
    fi
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm
    nvm install stable
    nvm alias default stable
    echo "nvm successfully installed"
fi

echo "Checking for prettier..."
if [[ $(command -v prettier) == "" || "$UPGRADE" ]]; then
    echo "Installing prettier"
    npm install -g --force prettier
    echo "Prettier successfully installed"
fi

# Go language server
echo "Checking go language server..."
if [[ $(command -v gopls) == "" ]]; then
    echo "Installing go language server..."
    GO111MODULE=on go get golang.org/x/tools/gopls@latest
    echo "go language server successfully installed"
fi

# shfmt
echo "Checking shfmt..."
if [[ $(command -v shfmt) == "" ]]; then
    echo "Installing shfmt..."
    GO111MODULE=on go get mvdan.cc/sh/v3/cmd/shfmt
    echo "shfmt successfully installed"
fi

echo "Checking org files..."
if [[ -e "$HOME/Dropbox/stow" ]]; then
    if [[ -e "$HOME/org" ]] && [[ ! -L "$HOME/org" ]]; then
        rm -rf "$HOME/org"
    fi
    stow --dir "$HOME/Dropbox/stow" --target "$HOME" org
else
    echo "Dropbox not yet synced. Rerun once ~/Dropbox/stow is available"
fi

echo "Done!"
