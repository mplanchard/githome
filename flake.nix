{
  description = "System config";

  inputs = {
    nixpkgs.url = "nixpkgs/release-22.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    # nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # must match nixpkgs version
    home-manager.url = "github:nix-community/home-manager/release-22.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # GL support on non nixOS systems
    # nixGL = { url = "github:guibou/nixGL"; flake = false; };
  };

  # `inputs@` stores extra arguments in the ... in a var called `inputs`
  outputs = inputs@{ self, emacs-overlay, home-manager, nixpkgs, nixpkgs-unstable, ... }:
    let

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

      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      # Call a function for all supported systems, generating an attrset
      # keyed by system
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      pkgs = forAllSystems (system:
        let
          unstable = import nixpkgs-unstable {
            inherit system;
            config.allowUnfree = true;
          };
          unstable-overlay = self: super: {
            _1password = unstable._1password;
            _1password-gui = unstable._1password-gui;
            discord = unstable.discord;
            gnomeExtensions = super.gnomeExtensions // {
              weather = unstable.gnomeExtensions.weather;
            };
            iosevka-comfy = unstable.iosevka-comfy.comfy;
            iosevka-comfy-motion = unstable.iosevka-comfy.comfy-motion;
            iosevka-comfy-motion-fixed = unstable.iosevka-comfy.comfy-motion-fixed;
            mu = unstable.mu;
            zoom-us = unstable.zoom-us;
          };
        in
          import nixpkgs {
            inherit system;
            overlays = overlays ++ [ unstable-overlay ];
            config.allowUnfree = true;
          }
      );

      unstable = forAllSystems (system:
        import nixpkgs-unstable {
          inherit system overlays;
          config.allowUnfree = true;
        }
      );

      combineHomeManagerModules = pkgs: modules:
        pkgs.lib.foldl
          (config: module: module { inherit pkgs; hmConfig = config; })
          ((pkgs.lib.head modules) { inherit pkgs; })
          (pkgs.lib.tail modules);

    in
    rec {
      mp-st-nix =
        let
          system = "x86_64-linux";
          systemPkgs = pkgs.x86_64-linux;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs [
            (import ./home-manager/base.nix)
            (import ./home-manager/not-aarch64.nix)
            (import ./home-manager/email.nix)
            (import ./home-manager/kde.nix)
          ];
          homeManager = (home-manager.lib.homeManagerConfiguration (
            {
              inherit system;
              pkgs = systemPkgs;
              homeDirectory = "/home/matthew";
              username = "matthew";
              # This is the home manager config. The attrset should be the same format
              # as what you find in home-manager's documentation for what you'd put in
              # a home.nix file.
              # configuration = homeManagerConfig;
              configuration = homeManagerConfig;
            })).activationPackage;
        };

      mp-mininix =
        let
          system = "x86_64-linux";
          systemPkgs = pkgs.x86_64-linux;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs [
            (import ./home-manager/base.nix)
            (import ./home-manager/not-aarch64.nix)
            (import ./home-manager/email.nix)
            (import ./home-manager/kde.nix)
          ];
          homeManager = (home-manager.lib.homeManagerConfiguration (
            {
              inherit system;
              pkgs = systemPkgs;
              homeDirectory = "/home/matthew";
              username = "matthew";
              # This is the home manager config. The attrset should be the same format
              # as what you find in home-manager's documentation for what you'd put in
              # a home.nix file.
              # configuration = homeManagerConfig;
              configuration = homeManagerConfig;
            })).activationPackage;
        };

      mp-st-m1 =
        let
          system = "aarch64-darwin";
          systemPkgs = pkgs.x86_64-darwin;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs [
            (import ./home-manager/base.nix)
            (import ./home-manager/email.nix)
          ];
          homeManager = (home-manager.lib.homeManagerConfiguration (
            {
              inherit system;
              pkgs = systemPkgs;
              homeDirectory = "/Users/matthew";
              username = "matthew";
              # This is the home manager config. The attrset should be the same format
              # as what you find in home-manager's documentation for what you'd put in
              # a home.nix file.
              # configuration = homeManagerConfig;
              configuration = homeManagerConfig;
            })).activationPackage;
        };

      nixosConfigurations = {
        mp-st-nix = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = pkgs.x86_64-linux;
          modules = [
            ./nixos/configuration-mp-st-nix.nix ({ pkgs, ... }: {
              services.clamav = {
                daemon.enable = true;
                updater.enable = true;
              };
            })
            ./nixos/crowdstrike-falcon-sensor/module.nix
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.matthew = mp-st-nix.homeManagerConfig;
            }
          ];
        };
        mininix = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = pkgs.x86_64-linux;
          modules = [
            ./nixos/configuration-mininix.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.matthew = mp-st-nix.homeManagerConfig;
            }
          ];
        };
      };
    };
}
