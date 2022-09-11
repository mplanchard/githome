# standard home-manager config that's the same on all machines
{ pkgs, ... }:

{
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
      initExtra = ''
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

        alias rdoc='xdg-open $(dirname $(which rustc))/../share/doc/rust/html/std/index.html'

        vterm_prompt_end(){
            vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
        }

        PWS=$(gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg || echo "fail")
        export GITLAB_TOKEN=$(echo "$PWS" | awk '/machine gitlab.com\/api login mplanchard/ { print $NF }')
        set_profile() {
            export AWS_PROFILE="$1"
        }
      '';
      shellAliases = {
        iam = "set_profile";
      };
    };

    bat.enable = true;

    exa = {
      enable = true;
      enableAliases = true;
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    # Always want emacs, this assumes the emacs overlay is present on pkgs
    emacs = {
      enable = true;
      package = pkgs.emacsPgtkNativeComp;
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

    nushell.enable = true;

    # Always install ssh
    ssh = {
      enable = true;
      matchBlocks = {
        "aws-ssm" = {
          host = "i-* mi-*";
          user = "admin";
          proxyCommand = ''
            sh -c "aws ssm start-session --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"
          '';
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };
      };
    };

    starship = {
      enable = true;
      # Enable once bash is configured by home manager
      enableBashIntegration = true;
      settings = {
        # Allow emacs-libvterm to jump between prompts
        format = "$all\\$\\(vterm_prompt_end\\)";
      };
    };

    # Install latex packages
    texlive.enable = true;

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
  };

  # Enable XDG env vars for config locations and such
  xdg.enable = true;

  home.packages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    bottom
    cachix
    cmake
    coreutils
    curl
    delta
    discord
    direnv
    emacs-all-the-icons-fonts
    fd
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
    ispell
    janet
    jq
    krita
    libreoffice
    lsof
    mitscheme
    neofetch
    neovim
    nerdfonts
    nodejs
    nodePackages.npm
    openssh
    pandoc
    pinentry
    pinentry-curses
    procps
    python3Full
    python3Packages.grip
    ripgrep
    rnix-lsp
    scheme-manpages
    shellcheck
    slack
    signal-desktop
    spotify
    sqlite
    stow
    tmux
    tokei
    unzip
    wget
    which
    yarn
    zip
    zoom-us
    zulip

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
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
      pinentryFlavor = "emacs";
    };
  };
  # Custom services
  systemd.user.services = {
    dropbox = {
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
      Unit = {
        Description = "dropbox";
      };
      Service = {
        ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
        ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
        KillMode = "control-group"; # upstream recommends process
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "full";
        Nice = 10;
      };
    };
  };
}
