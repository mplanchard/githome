{
  inputs,
  lib,
  config,
  pkgs,
  unstable,
  ...
}: {
  imports = [
    ./base.nix
    ./linux.nix
    ./not-aarch64.nix
    ./email.nix
    ./gnome.nix
    # inputs.stasis.homeModules.default
    ./niri.nix
  ];

  home.packages = [
    unstable.zed-editor
  ];
}
