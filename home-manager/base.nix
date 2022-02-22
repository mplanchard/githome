# standard home-manager config that's the same on all machines
{ pkgs, ... }:

let 
  hmConfig = {};
in
{
  # The general thing seems to be that if you want home-manager to manage
  # a program's config, use it as`programs.whatever` or `services.whatever`.
  # If you just want it available, stick it in packages.
  programs = hmConfig . programs or {} // {
    alacritty = hmConfig . programs.alacritty or {} // {
      enable = true;
      settings = {
        scrolling = {
          history = 10000;
          multiplier = 3;
        };
        font = {
          normal.family = "Fira Code";
          use_thin_strokes = true;
        };
        draw_bold_text_with_bright_colors = true;
        shell = {
          program = "tmux";
          args = [ "-l" ];
        };
      };
    };

    bash = hmConfig . programs.bash or {} // {
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

        vterm_prompt_end(){
            vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
        }

        PWS=$(gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg || echo "fail")
        export GITLAB_TOKEN=$(echo "$PWS" | awk '/machine gitlab.com\/api login mplanchard/ { print $NF }')
      '';
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
      # nix-direnv.enableFlakes = true;
    };

    # Always want emacs, this assumes the emacs overlay is present on pkgs
    emacs = {
      enable = true;
      package = (pkgs.emacsPgtkGcc.override {
        withXwidgets = true;
      });
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
  home.sessionPath = hmConfig . home.sessionPath or [] ++ [
    "$HOME/bin"
    "$HOME/.emacs.d/bin"
  ];

  home.sessionVariables = hmConfig . home.sessionVariables or {} // {
    # MOZ_ENABLE_WAYLAND = 1;
    MOZ_DBUS_REMOTE = 1;
    GITLAB_USER = "mplanchard";
    EDITOR = "emacsclient";
  };

  # Enable XDG env vars for config locations and such
  xdg.enable = true;

  home.packages = with pkgs; hmConfig . packages or [] ++ [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    bottom
    cachix
    cmake
    coreutils
    curl
    delta
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
    htmlTidy
    htop
    ispell
    janet
    jq
    lsof
    neovim
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
    shellcheck
    sqlite
    stow
    tmux
    tokei
    unzip
    wget
    which
    yarn
    zip

    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeType = "x-scheme-handler/org-protocol";
      icon = "emacs";
      terminal = false;
      categories = "System";
    })
  ];
} // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  services = hmConfig . services or {} // {
    emacs = { enable = true; };
    gpg-agent = {
      enable = true;
      # make it last all day
      defaultCacheTtl = 28800;
      defaultCacheTtlSsh = 28800;
      maxCacheTtl = 28800;
      maxCacheTtlSsh = 28800;
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
