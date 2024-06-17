{ config, lib, pkgs, ... }:

{
  users.users.matthew = {
    name = "matthew";
    home = "/Users/matthew";
  };
  services.nix-daemon.enable = true;

  # home-manager.useUserPackages = true;
  # home-manager.useGlobalPkgs = true;

  programs.fish.enable = true;

  nix.package = pkgs.nix;
  nix.settings.experimental-features = "nix-command flakes";

  nixpkgs.hostPlatform = "aarch64-darwin";
}
