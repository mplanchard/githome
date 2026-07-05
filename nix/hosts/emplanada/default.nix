{
  pkgs,
  ...
}:
{
  imports = [
    ../../../home-manager/core.nix
    ../../../home-manager/emacs.nix
    ../../../home-manager/maestral
  ];
}
