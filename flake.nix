{
  description = "System config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";
    # nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # GL support on non nixOS systems
    # nixGL = { url = "github:guibou/nixGL"; flake = false; };
  };

  # `inputs@` stores extra arguments in the ... in a var called `inputs`
  outputs = inputs@{ self, emacs-overlay, home-manager, nixpkgs, ... }: 
let 
    system = "x86_64-linux";
    # unstable = import inputs.nixpkgs-unstable { inherit system; };
    overlays = [
      emacs-overlay.overlay
      # Add in GL wrappers, used below.
      # ((import ./nixGL.nix) (import inputs.nixGL.outPath))
      # (final: prev: {
      #   # Resolve an issue where alacritty cannot find GL libraries since they
      #   # are system libraries.
      #   alacritty = prev.wrapWithNixGLIntel prev.alacritty;
      #   # There is some suggestion on GH that this should also work for zoom-us
      #   # (i.e. zoom), but haven't been able to figure it out for me:
      #   # https://github.com/NixOS/nixpkgs/issues/82959
      #   # zoom-us = prev.wrapWithNixGLIntel prev.zoom-us;
      # })
      # (self: super: {
      #   kalendar = unstable.kalendar;
      # })
    ];
    # Use nixpkgs for our system and with our specified overlays
    pkgs = import nixpkgs {
      inherit system overlays;
      config = {
        allowUnfree = true;
      };
    };

    homeManagerConfig = {

      # relative to ~
      accounts.email.maildirBasePath = ".mail";
      accounts.email.accounts.gmail = {
        address = "msplanchard@gmail.com";
        flavor = "gmail.com";
        maildir.path = "gmail";
        passwordCommand = "gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/machine imap.gmail.com login msplanchard@gmail.com password/ {print $NF}'";
        primary = true;
        realName = "Matthew Planchard";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = [
            "*"
            "![Gmail]*"
            "[Gmail]/Drafts"
            "[Gmail]/Sent Mail"
            "[Gmail]/Starred"
            "[Gmail]/All Mail"
          ];
          # local = {
          #   SubFolders = "Verbatim";
          # };
        };
      };

      accounts.email.accounts.spectrust = {
        address = "matthew@spec-trust.com";
        flavor = "gmail.com";
        maildir.path = "spectrust";
        passwordCommand = "gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/machine imap.gmail.com login matthew@spec-trust.com password/ {print $NF}'";
        realName = "Matthew Planchard";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
        };
      };

      # accounts.email.accounts.protonmail = {
      #   address = "inbox@mplanchard.com";
      #   maildir.path = "protonmail";
      #   passwordCommand = "gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/machine 127.0.0.1 login inbox@mplanchard.com password/ {print $NF}'";
      #   realName = "Matthew Planchard";
      #   mbsync = {
      #     enable = true;
      #     create = "both";
      #     expunge = "both";
      #   };
      # };

      # ensure nix programs can find nix-installed fonts
      fonts.fontconfig.enable = true;

      # Extra variables to add to PATH
      home.sessionPath = [
        "$HOME/bin"
        "$HOME/.emacs.d/bin"
      ];

      home.sessionVariables = {
        # MOZ_ENABLE_WAYLAND = 1;
        MOZ_DBUS_REMOTE = 1;
      };

      xdg.enable = true;


      # The general thing seems to be that if you want home-manager to manage
      # a program's config, use it as`programs.whatever` or `services.whatever`.
      # If you just want it available, stick it in packages.

      programs = {
        alacritty = {
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

        bash = {
          enable = true;
        };

        bat.enable = true;

        exa = {
          enable = true;
          # turn this on once I get bash config managed by home manager.
          enableAliases = true;
        };

        direnv = {
          enable = true;
          # turn this on once I get bash config managed by home manager.
          enableBashIntegration = true;
          nix-direnv.enable = true;
        };

        emacs = {
          enable = true;
          package = pkgs.emacsPgtkGcc;
          # automatically install vterm so we don't need to compile it in doom
          extraPackages = epkgs: [ epkgs.vterm ];
        };

        firefox = {
          enable = true;
          # package = pkgs.firefox-wayland.override {
          #   cfg = {
          #     enableGnomeExtensions = true;
          #   };
          # };
        };

        # letting home manager do this ensures that both nix-installed
        # and regular stuff is available to `info`.
        info.enable = true;

        mbsync = {
          enable = true;
          # extraConfig = ''
          #   SSLType IMAPS
          #   AuthMechs LOGIN
          # '';
        };

        # Ensure that home-manager installed packages have man pages
        man = {
          enable = true;
          # allow searching w/stuff like apropos
          generateCaches = true;
        };

        # mail stuff
        mu.enable = true;

        nix-index = {
          enable = true;
          # Enable once I get bash config into home manager
          enableBashIntegration = true;
        };

        nushell.enable = true;

        starship = {
          enable = true;
          # Enable once bash is configured by home manager
          enableBashIntegration = true;
        };

        texlive.enable = true;

      };

      services = {
        # dropbox = {
        #   enable = true;
        # };
        emacs = { enable = true; };
        gpg-agent = {
          enable = true;
          enableSshSupport = true;
          extraConfig = ''
            allow-emacs-pinentry
            allow-loopback-pinentry
          '';
          pinentryFlavor = "emacs";
        };
        # Automatically synchronize mail
        mbsync.enable = true;
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
            # Environment =
            # environment = {
            #   QT_PLUGIN_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtPluginPrefix;
            #   QML2_IMPORT_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtQmlPrefix;
            # };
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

      # home.file.".aspell.conf".text = "data-dir ${pkgs.aspell}/lib/aspell";

      # cmdline packages to be installed in the user env.
      home.packages = with pkgs; [
        _1password
        _1password-gui
        aspell
        aspellDicts.en
        # aspellDicts.en-computers
        automake
        bottom
        cmake
        coreutils
        curl
        delta
        direnv
        dropbox
        emacs-all-the-icons-fonts
        # libtool # for emacs (vterm)
        # libvterm # also for emacs
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
        libreoffice
        lld
        lldb
        llvm
        lsof
        neovim
        niv
        nodejs
        nodePackages.npm
        openssh
        pandoc
        pinentry
        pinentry-curses
        procps
        python3Full
        ripgrep
        shellcheck
        signal-desktop
        slack
        sqlite
        stow
        tmux
        wget
        which
        yarn
        zip
        zoom-us

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

        # kde-specific stuff
        plasma5Packages.accounts-qt
        plasma5Packages.akonadi
        plasma5Packages.akonadi-calendar
        plasma5Packages.calendarsupport
        plasma5Packages.kaccounts-integration
        plasma5Packages.kaccounts-providers
        plasma5Packages.kcalendarcore
        plasma5Packages.kcharselect
        plasma5Packages.kontact
        plasma5Packages.korganizer
        plasma5Packages.yakuake

        # protonmail
        pass
        protonmail-bridge


        # for sway
        # dmenu
        # foot
        # i3status-rust
        # mako
        # swaylock
        # swayidle
        # wl-clipboard
      ];
    };

    # wayland.windowManager.sway = {
    #   enable = true;
    #   wrapperFeatures.gtk = true;
    # };

in {

    nixosConfigurations = {
      mp-st-nix = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ 
          ./configuration.nix 
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.matthew = homeManagerConfig;
          }
        ];
      };
    };

    homeManagerBuild = (home-manager.lib.homeManagerConfiguration {
      inherit system pkgs;
      homeDirectory = "/home/matthew";
      username = "matthew";
      # This is the home manager config. The attrset should be the same format
      # as what you find in home-manager's documentation for what you'd put in
      # a home.nix file.
      configuration = homeManagerConfig;
    }).activationPackage;

    # Run the home-manager build when we run `nix build` for this directory.
    # defaultPackage.x86_64-linux = homeManagerConfig.activationPackage;
  };
}