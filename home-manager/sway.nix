{ hmConfig, pkgs, ... }:

hmConfig // {
  # programs = hmConfig.programs or {} // {
  #   swaylock = { enable = true; };
  # };
  # services = hmConfig.services or {} // {
  #   swayidle = { enable = true; };
  # };
  home = hmConfig.home or {} // {
    packages = with pkgs; hmConfig . home.packages or [] ++ [
      dmenu
      i3status-rust
      mako
      networkmanager
      pavucontrol
      pipewire
      playerctl
      pulseaudio
      swaylock
      xwayland
    ];
  };
  # wayland = hmConfig.wayland or {} // {
  #   windowManager = {
  #     sway = { enable = true; };
  #   };
  # };
}
