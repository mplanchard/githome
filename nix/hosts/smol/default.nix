{
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ./configuration.nix
    ./network.nix
    ./users
    ./transmission.nix
    ./plex.nix
    ./server.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.extraSpecialArgs = {
    inherit inputs;
  };
}
