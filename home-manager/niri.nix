{
  lib,
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    swaybg
    swayidle
    fuzzel
    foot
  ];
  programs.waybar = {
    enable = true;
    settings = {
      mainBar.layer = "top";
    };
    systemd.enable = true;
  };
  programs.niri.settings = {
    input.keyboard.xkb = {
      options = "caps:ctrl_shifted_capslock,altwin:swap_lalt_lwin,compose:rctrl";
    };
    environment = {
      DISPLAY = ":0";
    };
    spawn-at-startup = [
      { command = ["xwayland-satellite"]; }
      { command = ["waybar"]; }
      { command = ["mako"]; }
      { command = ["mako"]; }
    ];
    binds = {
      "Mod+T".action.spawn = "alacritty";
    };
  };
  # services.mako = {
  #   enable = true;
  # };
}
