{
  description = "System config";

  inputs = {
    nixpkgs.url = "nixpkgs/release-25.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    nixpkgs-previous.url = "nixpkgs/release-24.05";

    # nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # must match nixpkgs version
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # niri (tiling WM)
    niri.url = "github:sodiboo/niri-flake";
    niri.inputs.nixpkgs.follows = "nixpkgs";

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
      nixpkgs-previous,
      niri,
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

      previous = forAllSystems (system:
        import nixpkgs-previous {
          inherit system overlays;
          config.allowUnfree = true;
        }
      );

      combineHomeManagerModules = pkgs: unstable: previous: modules:
        pkgs.lib.foldl
          (config: module: module { inherit pkgs unstable previous; hmConfig = config; })
          ((pkgs.lib.head modules) { inherit pkgs unstable previous; })
          (pkgs.lib.tail modules);

    in
    rec {
      packages.x86_64-linux.secureframe-agent = pkgs.x86_64-linux.callPackage (import ./nixos/secureframe-agent) { };

      packages.x86_64-linux.nnd = pkgs.x86_64-linux.callPackage ./pkgs/nnd.nix { };

      x86-linux-gnome =
        let
          system = "x86_64-linux";
          systemPkgs = pkgs.x86_64-linux;
          systemUnstable = unstable.x86_64-linux;
          systemPrevious = previous.x86_64-linux;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs systemUnstable systemPrevious [
            (import ./home-manager/base.nix)
            (import ./home-manager/linux.nix)
            (import ./home-manager/not-aarch64.nix)
            (import ./home-manager/email.nix)
            (import ./home-manager/gnome.nix)
          ];

          homeManager = (home-manager.lib.homeManagerConfiguration  {
              # inherit system;
              pkgs = systemPkgs;
              # configuration = homeManagerConfig;
              modules = [
                ({...}: homeManagerConfig)
                # niri.homeModules.config
                # ./home-manager/niri.nix
              ];
            });
        };

      mp-mininix =
        let
          system = "x86_64-linux";
          systemPkgs = pkgs.x86_64-linux;
          systemUnstable = unstable.x86_64-linux;
          systemPrevious = previous.x86_64-linux;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs systemUnstable systemPrevious [
            (import ./home-manager/base.nix)
            (import ./home-manager/linux.nix)
            (import ./home-manager/not-aarch64.nix)
            (import ./home-manager/email.nix)
            (import ./home-manager/kde.nix)
          ];
        };

      mp-st-m1 =
        let
          system = "aarch64-darwin";
          systemPkgs = pkgs.aarch64-darwin;
          systemUnstable = unstable.aarch64-darwin;
          systemPrevious = previous.x86_64-linux;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs systemUnstable systemPrevious [
            (import ./home-manager/base.nix)
            (import ./home-manager/email.nix)
          ];
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

      homeConfigurations = {
        "matthew@mp-st-nix-fw" =
        let
          system = "x86_64-linux";
          systemPkgs = pkgs.x86_64-linux;
          systemUnstable = unstable.x86_64-linux;
          systemPrevious = previous.x86_64-linux;
        in
        rec {
          homeManagerConfig = combineHomeManagerModules systemPkgs systemUnstable systemPrevious [
            (import ./home-manager/base.nix)
            (import ./home-manager/linux.nix)
            (import ./home-manager/not-aarch64.nix)
            (import ./home-manager/email.nix)
            (import ./home-manager/gnome.nix)
          ];

          homeManager = (home-manager.lib.homeManagerConfiguration  {
              # inherit system;
              pkgs = systemPkgs;
              configuration = homeManagerConfig;
              modules = [
                ({...}: homeManagerConfig)
                # niri.homeModules.config
                # ./home-manager/niri.nix
              ];
            });
        }.homeManager;
      };
      nixosConfigurations = {
        mp-st-nix-fw = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = pkgs.x86_64-linux;
          modules = [
            ./nixos/configuration-mp-st-nix-fw.nix
            nixos-hardware.nixosModules.framework-13th-gen-intel
            ./nixos/crowdstrike-falcon-sensor/module.nix
            # ./nixos/secureframe-agent/module.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
            {
              home-manager.users.matthew = x86-linux-gnome.homeManagerConfig;
            }
            {
              home-manager.users.matthew = (import ./home-manager/niri.nix);
            }
            niri.nixosModules.niri
            ({pkgs, ...}: {
              nixpkgs.overlays = [ niri.overlays.niri ];
              programs.niri.enable = true;
              environment.variables.NIXOS_OZONE_WL = "1";
              environment.systemPackages = with pkgs; [
                wl-clipboard
                wayland-utils
                xwayland-satellite
                xdg-desktop-portal-gtk
              ];
              # systemd.user.services.niri.wants =
            })
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
