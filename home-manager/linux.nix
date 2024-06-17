{ hmConfig, pkgs, ... }:

hmConfig // {
  home = hmConfig.home or {} // {
    packages = with pkgs; hmConfig . home.packages or [] ++ [
      gnupg
      # krita
      # libreoffice
      powertop
      tlp # power management
    ];
  };
}
