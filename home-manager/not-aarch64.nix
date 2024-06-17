{ hmConfig, pkgs, unstable, ... }:

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
      unstable._1password
      unstable._1password-gui
      appimage-run
      dmidecode
      dropbox
      libinput-gestures
      niv
      unstable.signal-desktop
      xdg-utils
      xorg.xev
      zulip
    ];
  };

}
