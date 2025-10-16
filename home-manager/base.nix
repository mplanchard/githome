# standard home-manager config that's the same on all machines
{ pkgs, unstable, previous, ... }:
{
  home.username = "matthew";
  home.homeDirectory = "/home/matthew";
  home.stateVersion = "22.11";
  programs.firefox = {
    enable = true;
    nativeMessagingHosts = [ unstable.firefoxpwa ];
  };
  # The general thing seems to be that if you want home-manager to manage
  # a program's config, use it as`programs.whatever` or `services.whatever`.
  # If you just want it available, stick it in packages.
  programs = {
    alacritty = {
      enable = true;
      settings = {
        draw_bold_text_with_bright_colors = true;
        font = {
          normal.family = "Fira Code";
          use_thin_strokes = true;
        };
        scrolling = {
          history = 100000;
          multiplier = 3;
        };
        shell = {
          program = "tmux";
          args = [ "-l" ];
        };
        window = {
          decorations = "full";
          gtk_theme_variant = "dark";
        };
      };
    };

    bash = {
      enable = true;
      profileExtra = ''
        if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
           source "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi
      '';
      initExtra = ''
        # Bind ctrl-backspace / ctrl-H to backward-kill word
        bind \cH backward-kill-word

        # HACK: for some reason this, unlike hte other sessionVariables, is getting
        # overridden in my shells. Set it manually.
        export EDITOR="emacsclient"
        # Setting this env var to the empty string makes it so that emacs
        # will start a daemon if none is running when trying to execute emacsclient
        export ALTERNATE_EDITOR=
        vterm_printf(){
          if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
        }
        vterm_prompt_end(){
            vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
        }
        starship_precmd_user_func="vterm_prompt_end"

        alias rdoc='xdg-open $(dirname $(which rustc))/../share/doc/rust/html/std/index.html'

        PWS=$(gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg || echo "fail")
        export GITLAB_TOKEN=$(echo "$PWS" | awk '/machine gitlab\.com\/api login mplanchard/ { print $NF }')
        export GITHUB_TOKEN=$(echo "$PWS" | awk '/machine api\.github\.com login mplanchard\^forge/ { print $NF }')
        export CACHIX_AUTH_TOKEN="$(echo "$PWS" | awk '/machine app\.cachix\.org login mplanchard/ { print $NF }')"
        set_profile() {
            export AWS_PROFILE="$1"
        }
      '';
      shellAliases = {
        iam = "set_profile";
      };
    };

    bat.enable = true;

    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      # seems to be auto-enabled for fish if direnv is enabled
      # enableFishIntegration = true;
      nix-direnv.enable = true;
    };

    fish = {
      enable = true;
      functions = {};
      shellAliases = {
        iam = "set_profile";
      };
      shellInit = ''
        if test -f ~/.authinfo.gpg
          set PWS "$(gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg || echo "fail")"
          set -gx GITLAB_TOKEN "$(echo "$PWS" | awk '/machine gitlab\.com\/api login mplanchard/ { print $NF }')"
          set -gx GITHUB_TOKEN "$(echo "$PWS" | awk '/machine api\.github\.com login mplanchard\^forge/ { print $NF }')"
          set -gx CACHIX_AUTH_TOKEN "$(echo "$PWS" | awk '/machine app\.cachix\.org login mplanchard/ { print $NF }')"
        end
        set -gx EDITOR "emacsclient"
        # Setting this env var to the empty string makes it so that emacs
        # will start a daemon if none is running when trying to execute emacsclient
        set -gx ALTERNATE_EDITOR ""

        function set_aws_profile
          set -gx AWS_PROFILE $argv[1]
        end

        if test 'vterm' = "$INSIDE_EMACS"
          source "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"
        end

        function vterm_finish --on-event fish_prompt
          if test 'vterm' = "$INSIDE_EMACS"; and test -n "$STARSHIP_SESSION_KEY"
            vterm_prompt_end
          end
        end

        abbr iam set_aws_profile
      '';
    };

    eza = {
      enable = true;
    };

    # Always want emacs, this assumes the emacs overlay is present on pkgs
    emacs = {
      enable = true;
      package = pkgs.emacs;
      extraPackages = epkgs: with epkgs; [
        mbsync
        mu4e
        vterm
        (treesit-grammars.with-grammars (grammars: with grammars; [
            tree-sitter-bash
            tree-sitter-clojure
            tree-sitter-comment
            tree-sitter-cpp
            tree-sitter-css
            tree-sitter-dockerfile
            tree-sitter-elisp
            tree-sitter-fish
            tree-sitter-go
            tree-sitter-graphql
            tree-sitter-hcl
            tree-sitter-html
            tree-sitter-janet-simple
            tree-sitter-javascript
            tree-sitter-jsdoc
            tree-sitter-json
            tree-sitter-json5
            tree-sitter-latex
            tree-sitter-lua
            tree-sitter-make
            tree-sitter-markdown
            tree-sitter-markdown-inline
            tree-sitter-nix
            tree-sitter-prisma
            tree-sitter-rust
            tree-sitter-scss
            tree-sitter-sql
            tree-sitter-svelte
            tree-sitter-toml
            tree-sitter-tsx
            tree-sitter-typescript
            tree-sitter-vim
            tree-sitter-yaml
        ]))
      ];
    };

    ghostty.enable = true;

    # letting home manager do this ensures that both nix-installed
    # and regular stuff is available to `info`.
    info.enable = true;

    # Ensure that home-manager installed packages have man pages
    man = {
      enable = true;
      # allow searching w/stuff like apropos
      generateCaches = true;
    };

    nushell.enable = true;

    readline = {
      enable = true;
      bindings = {
        "\\C-\\b" = "backward-kill-word";
        "\\C-\\d" = "backward-kill-word";
      };
    };

    # Always install ssh
    ssh = {
      enable = true;
      serverAliveInterval = 60;
      matchBlocks = {
        "gitlab" = {
          host = "gitlab.com";
          hostname = "altssh.gitlab.com";
          user = "git";
          port = 443;
        };
      };
    };

    starship = {
      enable = true;
      # Enable once bash is configured by home manager
      enableBashIntegration = true;
      enableFishIntegration = true;
      enableTransience = true;
      settings = {
        kubernetes.disabled = false;
        # get rid of annoying extra space
        nix_shell.format = "via [$symbol$state($name)]($style) ";
        nix_shell.symbol = "❄️ ";
        nix_shell.impure_msg = "";
        nix_shell.pure_msg = "pure ";
      };
    };

    zsh.enable = true;
    zsh.profileExtra = ''
        if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
           source "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi
    '';

    # Install latex packages
    # texlive.enable = true;
  };

  home.file.gdbinit = {
    target = ".gdbinit";
    text = ''
    set auto-load safe-path /nix/store/
    '';
  };

  # ensure nix programs can find nix-installed fonts
  fonts.fontconfig.enable = true;

  # ensure home-manager version matches nixpkgs version
  home.enableNixpkgsReleaseCheck = true;

  # Extra variables to add to PATH
  home.sessionPath = [
    "$HOME/bin"
    "$HOME/.emacs.d/bin"
  ];

  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    MOZ_DBUS_REMOTE = 1;
    GITLAB_USER = "mplanchard";
    EDITOR = "emacsclient";
    ALTERNATE_EDITOR = "";
    SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
    GSM_SKIP_SSH_AGENT_WORKAROUND = 1;
  };

  # Enable XDG env vars for config locations and such
  xdg.enable = true;

  home.packages = with pkgs; [
    unstable.aider-chat-full
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    bottom
    cachix
    # cargo
    chromium
    cmake
    coreutils
    curl
    delta
    discord
    # direnv
    unstable.element-desktop
    emacs-lsp-booster
    emacs-all-the-icons-fonts
    unstable.fd
    file
    findutils
    fira-code
    unstable.firefoxpwa
    fontconfig
    fzf
    gawk
    gcc
    ghostty
    git
    gnugrep
    gnumake
    hack-font
    html-tidy
    htop
    (hunspellWithDicts [
      hunspellDicts.en_US-large
    ])
    unstable.iosevka-comfy.comfy
    unstable.iosevka-comfy.comfy-motion
    unstable.iosevka-comfy.comfy-motion-fixed
    ispell
    unstable.janet
    unstable.jpm
    jq
    lsof
    mu.mu4e
    neovim
    nerd-fonts.code-new-roman
    nerd-fonts.envy-code-r
    nerd-fonts.hack
    nerd-fonts.jetbrains-mono
    nerd-fonts.monofur
    nerd-fonts.monoid
    nerd-fonts.mononoki
    nerd-fonts.ubuntu-mono
    nerd-fonts.ubuntu-sans
    nerd-fonts.recursive-mono
    nerd-fonts.sauce-code-pro
    nerd-fonts.space-mono
    nerd-fonts.tinos
    nerd-fonts.zed-mono
    nixfmt-rfc-style
    nodejs
    ollama
    # nodePackages.npm
    openssh
    pandoc
    pass
    pinentry
    # pinentry-curses
    procps
    python3Full
    ripgrep
    # rnix-lsp
    rustup
    scheme-manpages
    shellcheck
    slack
    spotify
    sqlite
    stow
    texlive.combined.scheme-full
    tmux
    tokei
    tree
    unstable.uhk-agent
    unzip
    wget
    which
    yarn
    unstable.zed-editor
    zip
    previous.zoom-us

    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeTypes = [ "x-scheme-handler/org-protocol" ];
      icon = "emacs";
      terminal = false;
      categories = [ "System" ];
    })
  ];
} // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  services = {
    # emacs = { enable = true; };
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 43200;  # 12 hours
      defaultCacheTtlSsh = 43200;
      maxCacheTtl = 43200;
      maxCacheTtlSsh = 43200;
      enableSshSupport = true;
      pinentry.package = pkgs.pinentry-gnome3;
      # extraConfig = ''
      #   allow-emacs-pinentry
      #   allow-loopback-pinentry
      #   pinentry-program /etc/profiles/per-user/matthew/bin/pinentry
      # '';
      # pinentryFlavor = "gnome3";
    };
  };
}
