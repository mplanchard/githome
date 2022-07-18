{ hmConfig, pkgs, ... }:

hmConfig // {
  programs = hmConfig . programs or {} // {
    firefox = {
      enable = true;
    };

    nix-index = {
      enable = true;
      # Enable once I get bash config into home manager
      enableBashIntegration = true;
      enableZshIntegration = true;
    };
  };

  home = hmConfig . home or {} // {
    packages = with pkgs; hmConfig . home.packages or [] ++ [
      _1password
      _1password-gui
      appimage-run
      dmidecode
      dropbox
      libinput-gestures
      niv
      xdg-utils
      xorg.xev
    ];
  };

}
