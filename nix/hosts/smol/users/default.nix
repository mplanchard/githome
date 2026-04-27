{ pkgs, ... }:
{
  home-manager.users.matthew = import ./matthew.nix;
}
