{ lib, pkgs, ... }:

let
  xkbOptions = [
    "altwin:swap_lalt_lwin"
    "caps:ctrl_modifier"
    "compose:rctrl"
    "shift:both_capslock"
    "terminate:ctrl_alt_bksp"
  ];
in
{
  imports = [];
  options = {};
  config = {
    console.useXkbConfig = true;
    services.xserver.xkb.options = lib.concatStringsSep "," xkbOptions;
    programs.dconf = {
      enable = true;
      profiles.user.databases = [
        {
          lockAll = true;
          settings = with lib.gvariant; {
            "org/gnome/desktop/input-sources" = {
              xkb-options = mkArray xkbOptions;
            };
            "org/gnome/desktop/wm/keybindings" = {
              maximize = mkArray [ "<Super>m" ];
              minimize = mkEmptyArray type.string;
              move-to-monitor-left = mkArray [ "<Control><Super>h" ];
              move-to-monitor-right = mkArray [ "<Control><Super>l" ];
              move-to-workspace-left = mkArray [ "<Shift><Control><Super>h" ];
              move-to-workspace-right = mkArray [ "<Shift><Control><Super>l" ];
              switch-to-workspace-left = mkArray [ "<Shift><Super>h" ];
              switch-to-workspace-right = mkArray [ "<Shift><Super>l" ];
              toggle-fullscreen = mkArray [ "<Super>f" ];
            };
            "org/gnome/mutter" = {
              edge-tiling = mkValue true;
            };
            "org/gnome/mutter/keybindings" = {
              toggle-tiled-left = mkArray [ "<Super>h" ];
              toggle-tiled-right = mkArray [ "<Super>l" ];
            };
            "org/gnome/settings-daemon/plugins/media-keys" = {
              screensaver = mkEmptyArray type.string;
            };
            "org/gnome/shell/keybindings" = {
              toggle-message-tray = mkEmptyArray type.string;
            };
          };
        }
      ];
    };
  };
}
