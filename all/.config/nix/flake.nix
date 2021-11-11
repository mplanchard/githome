{
  description = "System config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  # `inputs@` stores extra arguments in the ... in a var called `inputs`
  outputs = inputs@{ self, nixpkgs, ... }: rec {

    system = "x86_64-linux";

    pkgs = import nixpkgs.legacyPackages.x86_64-linux {
      inherit system;
      config.allowUnfree = true;
      overlays = [];
    };

    defaultPackage.${system} = import ./default.nix { config = pkgs.config; lib = pkgs.lib; pkgs = pkgs; };
  };
}
