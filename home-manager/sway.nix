{ hmConfig, pkgs, ... }:

hmConfig // {
  programs = hmConfig.programs or {} // {
    swayload = { enable = true; };
  };
  services = hmConfig.services or {} // {
    swayidle = { enable = true; };
  };
  home = hmConfig.home or {} // {
    packages = with pkgs; hmConfig . home.packages or [] ++ [

    ];
  };
  wayland = hmConfig.wayland or {} // {
    windowManager = {
      sway = { enable = true; };
    };
  };
}
