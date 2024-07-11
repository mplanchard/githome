# standard home-manager config that's the same on all machines
{ pkgs, unstable, ... }:

{
  home.stateVersion = "22.11";
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

    zsh.enable = true;
    zsh.profileExtra = ''
        if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
           source "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi
    '';

    bash = {
      enable = true;
      profileExtra = ''
        if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
           source "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi
      '';
      initExtra = ''
        # HACK: for some reason this, unlike hte other sessionVariables, is getting
        # overridden in my shells. Set it manually.
        export EDITOR=emacsclient
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
        set -gx EDITOR emacsclient

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

    bat.enable = true;

    eza = {
      enable = true;
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      # seems to be auto-enabled for fish if direnv is enabled
      # enableFishIntegration = true;
      nix-direnv.enable = true;
    };

    # Always want emacs, this assumes the emacs overlay is present on pkgs
    emacs = {
      enable = true;
      package = pkgs.emacs-git;
      # automatically install vterm so we don't need to compile it in doom
      extraPackages = epkgs: [ epkgs.vterm ];
    };

    # letting home manager do this ensures that both nix-installed
    # and regular stuff is available to `info`.
    info.enable = true;

    # Ensure that home-manager installed packages have man pages
    man = {
      enable = true;
      # allow searching w/stuff like apropos
      generateCaches = true;
    };

    # nushell.enable = true;

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
    SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
    GSM_SKIP_SSH_AGENT_WORKAROUND = 1;
  };

  # Enable XDG env vars for config locations and such
  xdg.enable = true;

  home.packages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    bottom
    cachix
    cargo
    cmake
    coreutils
    curl
    delta
    discord
    # direnv
    unstable.element-desktop
    emacs-all-the-icons-fonts
    unstable.fd
    findutils
    fira-code
    fontconfig
    fzf
    gawk
    gcc
    git
    gnugrep
    gnumake
    hack-font
    html-tidy
    htop
    unstable.iosevka-comfy.comfy
    unstable.iosevka-comfy.comfy-motion
    unstable.iosevka-comfy.comfy-motion-fixed
    ispell
    janet
    jq
    lsof
    mu.mu4e
    neovim
    nerdfonts
    nodejs
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
    # rustc
    # rust-analyzer
    scheme-manpages
    shellcheck
    slack
    spotify
    sqlite
    stow
    texlive.combined.scheme-full
    tmux
    tokei
    unzip
    wget
    which
    yarn
    unstable.zed-editor
    zip
    zoom-us

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
    emacs = { enable = true; };
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 43200;  # 12 hours
      defaultCacheTtlSsh = 43200;
      maxCacheTtl = 43200;
      maxCacheTtlSsh = 43200;
      enableSshSupport = true;
      pinentryPackage = pkgs.pinentry-gnome3;
      # extraConfig = ''
      #   allow-emacs-pinentry
      #   allow-loopback-pinentry
      #   pinentry-program /etc/profiles/per-user/matthew/bin/pinentry
      # '';
      # pinentryFlavor = "gnome3";
    };
  };
}
