{
  description = "System config";

  inputs = {
    nixpkgs.url = "nixpkgs/release-24.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    # nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # must match nixpkgs version
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # standard, device-specific hardware tweaks
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    # GL support on non nixOS systems
    # nixGL = { url = "github:guibou/nixGL"; flake = false; };
  };

  # `inputs@` stores extra arguments in the ... in a var called `inputs`
  outputs = inputs@{
      self,
      emacs-overlay,
      home-manager,
      nix-darwin,
      nixos-hardware,
      nixpkgs,
      nixpkgs-unstable,
      ...
  }:
    let

      # unstable = import inputs.nixpkgs-unstable { inherit system; };
      overlays = [ emacs-overlay.overlay ];

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
        in
          import nixpkgs {
            inherit system overlays;
            config.allowUnfree = true;
          }
      );

      unstable = forAllSystems (system:
        import nixpkgs-unstable {
          inherit system overlays;
          config.allowUnfree = true;
        }
      );

      combineHomeManagerModules = pkgs: unstable: modules:
        pkgs.lib.foldl
          (config: module: module { inherit pkgs unstable; hmConfig = config; })
          ((pkgs.lib.head modules) { inherit pkgs unstable; })
          (pkgs.lib.tail modules);

    in
    rec {
      packages.x86_64-linux.secureframe-agent = pkgs.x86_64-linux.callPackage (import ./nixos/secureframe-agent) { };

      x86-linux-gnome =
        let
          system = "x86_64-linux";
          systemPkgs = pkgs.x86_64-linux;
          systemUnstable = unstable.x86_64-linux;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs systemUnstable [
            (import ./home-manager/base.nix)
            (import ./home-manager/linux.nix)
            (import ./home-manager/not-aarch64.nix)
            (import ./home-manager/email.nix)
            (import ./home-manager/gnome.nix)
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
              configuration = homeManagerConfig;
            })).activationPackage;
        };

      mp-mininix =
        let
          system = "x86_64-linux";
          systemPkgs = pkgs.x86_64-linux;
          systemUnstable = unstable.x86_64-linux;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs systemUnstable [
            (import ./home-manager/base.nix)
            (import ./home-manager/linux.nix)
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
              configuration = homeManagerConfig;
            })).activationPackage;
        };

      mp-st-m1 =
        let
          system = "aarch64-darwin";
          systemPkgs = pkgs.aarch64-darwin;
          systemUnstable = unstable.aarch64-darwin;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs systemUnstable [
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
              configuration = homeManagerConfig;
            })).activationPackage;
        };

      darwinConfigurations = {
        mp-st-m1 = nix-darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./darwin.nix
            home-manager.darwinModules.home-manager {
              # home-manager.useGlobalPkgs = true;
              # home-manager.useUserPackages = true;
              home-manager.users.matthew = mp-st-m1.homeManagerConfig;
            }
          ];
        };
      };

      nixosConfigurations = {
        mp-st-nix-fw = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = pkgs.x86_64-linux;
          modules = [
            ./nixos/configuration-mp-st-nix-fw.nix
            nixos-hardware.nixosModules.framework-13th-gen-intel
            ./nixos/crowdstrike-falcon-sensor/module.nix
            ./nixos/secureframe-agent/module.nix
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.matthew = x86-linux-gnome.homeManagerConfig;
            }
            ./nixos/gnome.nix
          ];
        };
        mp-st-nix = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = pkgs.x86_64-linux;
          modules = [
            ./nixos/configuration-mp-st-nix.nix
            ./nixos/crowdstrike-falcon-sensor/module.nix
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.matthew = x86-linux-gnome.homeManagerConfig;
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
              home-manager.users.matthew = x86-linux-gnome.homeManagerConfig;
            }
          ];
        };
      };
    };
}
