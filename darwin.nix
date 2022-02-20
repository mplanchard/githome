{ config, lib, pkgs, ... }:

{
  users.users.matthew = {
    name = "matthew";
    home = "/Users/matthew";
  };
  home-manager.users.matthew = import ./home-manager/base.nix;
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
}
