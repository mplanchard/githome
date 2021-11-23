{
  description = "System config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # GL support on non nixOS systems
    nixGL.url = "github:guibou/nixGL";
  };

  # `inputs@` stores extra arguments in the ... in a var called `inputs`
  outputs = inputs@{ self, emacs-overlay, home-manager, nixpkgs, nixGL, ... }: rec {

    system = "x86_64-linux";

    overlays = [
      emacs-overlay.overlay
      # Add in GL wrappers, used below.
      ((import ./nixGL.nix) (import inputs.nixGL.outPath))
      (final: prev: {
        # Resolve an issue where alacritty cannot find GL libraries since they
        # are system libraries.
        alacritty = prev.wrapWithNixGLIntel prev.alacritty;
        # There is some suggestion on GH that this should also work for zoom-us
        # (i.e. zoom), but it doesn't for me. See discussion here: https://github.com/NixOS/nixpkgs/issues/82959
      })
    ];

    # Use nixpkgs for our system and with our specified overlays
    pkgs = import nixpkgs {
      inherit system overlays;
      config = {
        allowUnfree = true;
      };
    };

    homeManagerConfig = home-manager.lib.homeManagerConfiguration {
      inherit system pkgs;
      homeDirectory = "/home/matthew";
      username = "matthew";
      # This is the home manager config. The attrset should be the same format
      # as what you find in home-manager's documentation for what you'd put in
      # a home.nix file.
      configuration = {

        # ensure nix programs can find nix-installed fonts
        fonts.fontconfig.enable = true;

        # Extra variables to add to PATH
        home.sessionPath = [
          "$HOME/bin"
        ];

        home.sessionVariables = {
          MOZ_ENABLE_WAYLAND = 1;
          MOZ_DBUS_REMOTE = 1;
        };


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

          bat.enable = true;

          exa = {
            enable = true;
            # turn this on once I get bash config managed by home manager.
            # enableAliases = true;
          };

          direnv = {
            enable = true;
            # turn this on once I get bash config managed by home manager.
            # enableBashIntegration = true;
            nix-direnv.enable = true;
          };

          emacs = {
            enable = true;
            package = pkgs.emacsPgtkGcc;
          };

          firefox = {
            enable = true;
            package = pkgs.firefox-wayland.override {
              cfg = {
                enableGnomeExtensions = true;
              };
            };
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

          nix-index = {
            enable = true;
            # Enable once I get bash config into home manager
            # enableBashIntegration = true;
          };

          nushell.enable = true;

          starship = {
            enable = true;
            # Enable once bash is configured by home manager
            # enableBashIntegration = true;
          };

          texlive.enable = true;
        };

        services = {
          dropbox = {
            enable = true;
          };
          emacs = { enable = true; };
          gpg-agent = {
            enable = true;
            enableSshSupport = true;
            extraConfig = ''
              allow-emacs-pinentry
              allow-loopback-pinentry
            '';
            pinentryFlavor = "gnome3";
          };
        };

        # cmdline packages to be installed in the user env.
        home.packages = with pkgs; [
          automake
          bash
          bottom
          cmake
          coreutils
          curl
          direnv
          evolution
          fd
          findutils
          fira-code
          fontconfig
          foot  # super fast wayland terminal
          fzf
          gawk
          gcc
          git
          gnugrep
          gnumake
          hack-font
          htmlTidy
          htop
          janet
          jq
          libreoffice
          lld
          lldb
          llvm
          neovim
          niv
          nodejs
          nodePackages.npm
          openssh
          pandoc
          procps
          python3Full
          ripgrep
          shellcheck
          signal-desktop
          sqlite
          stow
          wget
          which
          yarn
          zip
        ];
      };
    };

    # Run the home-manager build when we run `nix build` for this directory.
    defaultPackage.x86_64-linux = homeManagerConfig.activationPackage;
  };
}
