{ hmConfig, pkgs, ... }:

hmConfig // {
  home = hmConfig.home or {} // {
    packages = with pkgs; hmConfig . home.packages or [] ++ [
      gnomeExtensions.appindicator
      gnome.gnome-shell-extensions
      gnome.gnome-tweaks
      pavucontrol
    ];
  };
}
