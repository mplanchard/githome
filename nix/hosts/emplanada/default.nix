{
  pkgs,
  ...
}:
{
  imports = [
    ../../../home-manager/core.nix
    ../../../home-manager/emacs.nix
    ../../../home-manager/fonts.nix
    ../../../home-manager/maestral
    ../../../home-manager/shell.nix
  ];
}
