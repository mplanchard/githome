{
  lib,
  pkgs,
  config,
  inputs,
  ...
}:
{
  imports = [
    inputs.stasis.homeModules.default
    inputs.dms.homeModules.dank-material-shell
    inputs.dms.homeModules.niri
  ];
  programs.dank-material-shell = {
    enable = true;
    systemd = {
      enable = true;
      restartIfChanged = true;
    };
    dgop.package = inputs.dgop.packages.${pkgs.system}.default;
    enableSystemMonitoring = true;
    enableVPN = true;
    enableDynamicTheming = true;
    enableAudioWavelength = true;
    enableCalendarEvents = true;
    enableClipboardPaste = true;
    niri = {
      # enableKeybinds = true;
      includes = {
        enable = true;
        override = false;
      };
      # enableSpawn = true;
    };
  };
#   services.stasis = {
#     enable = true;
#     extraPathPackages = with pkgs; [
#       brightnessctl
#       niri
#       swaylock
#     ];
#     extraConfig = ''
#       @author "Dustin Pilgrim"
#       @description "Lightweight feature packed idle manager for Wayland"

#       # Semantics:
#       # - Everything lives under `default:` (and optional profile blocks).
#       # - On laptops, Stasis chooses between:
#       #     `default.ac:` and `default.battery:`
#       #   depending on current power source.
#       # - Lid actions live under `default:` (globals) so they apply on BOTH AC and Battery
#       #   unless a profile overlays/clears them.

#       default:
#         # Optional: listen for loginctl lock/unlock-session signals (default false)
#         # NOTE: this only updates internal state when lock/unlock signals are received;
#         # it does not actually run your locker command.
#         #enable_loginctl true

#         # Optional: listen for session D-Bus inhibit traffic (default true).
#         # This gate is not browser-only: browsers, Steam, and other desktop apps can
#         # request inhibit through standard D-Bus/portal paths.
#         # Browser/media-tab inhibit lives here, not under monitor_media.
#         # Disable this only if you explicitly do not want D-Bus inhibit requests
#         # to pause idle progression.
#         #enable_dbus_inhibit false

#         # Optional: run before suspending (hook).
#         #
#         # Behavior:
#         # - This command runs RIGHT BEFORE the suspend step, and Stasis currently WAITS
#         #   for it to exit before continuing to suspend.
#         # - Therefore, a foreground screen locker (e.g. `swaylock -F`, `hyprlock`) will
#         #   BLOCK suspend unless it daemonizes / returns quickly.
#         # - Also: do NOT run your locker BOTH here and in `lock_screen.command`.
#         #   If you already have a lock step, leave `pre_suspend_command` unset.
#         #
#         # Good uses:
#         #   pre_suspend_command "sync"
#         #   pre_suspend_command "~/.local/bin/my-pre-sleep-hook"
#         #
#         # If you do NOT have a lock step and you want "lock right before suspend",
#         # use a locker that exits immediately (daemon/background), e.g. via a wrapper script.
#         #pre_suspend_command "sync"

#         # Non-browser media/audio inhibit only. Browser/media-tab inhibit is handled
#         # by enable_dbus_inhibit above.
#         monitor_media true
#         ignore_remote_media true # ignore remote players (spotify/kdeconnect/etc.)

#         # Optional: ignore these media sources for media inhibit (case-insensitive)
#         #media_blacklist ["spotify"]

#         # Debounce window in seconds before starting the plan (default 0)
#         #debounce_seconds 4

#         # Notify when resuming from IPC pause (e.g., `stasis pause 1h`)
#         #notify_on_unpause true

#         # Global gate: notifications before steps only happen if this is true
#         #notify_before_action true

#         # App/process inhibit patterns (strings or regex literals)
#         inhibit_apps [
#           "vlc"
#           "mpv"
#           r"steam_app_.*"
#         ]

#         # -----------------------------
#         # Prepare sleep command
#         # -----------------------------
#         # Shell command to run immediately when logind emits PrepareForSleep(true).
#         # Unlike `pre_suspend_command`, this triggers when sleep is initiated
#         # externally (e.g., closing lid, systemctl suspend), not by stasis idle plan.
#         #
#         # Examples:
#         #   prepare_sleep_command "swaylock -f"
#         # -----------------------------
#         #prepare_sleep_command ""

#         # -----------------------------
#         # Lid actions (LAPTOP ONLY)
#         #
#         # Shell commands run immediately on lid close or open.
#         # Lid close/open also pause/resume the plan timers regardless.
#         #
#         # These are GLOBAL under `default:` so they apply to BOTH `ac:` and `battery:` plans.
#         # A profile can override them or clear them (set to "").
#         #
#         # Examples:
#         #   lid_close_action "swaylock"
#         #   lid_open_action  "brightnessctl set 60%"
#         # -----------------------------
#         #lid_close_action "swaylock"
#         #lid_open_action  ""

#         # Laptop plan: AC power (relaxed)
#         ac:
#           brightness:
#             timeout 300 # 5 minute(s)
#             command "${pkgs.brightnessctl}/bin/brightnessctl set 50%"
#           end

#           dpms:
#             timeout 120 # 2 minute(s) after brightness
#             command "${pkgs.niri}/bin/niri msg action power-off-monitors"
#             resume_command "${pkgs.niri}/bin/niri msg action power-on-monitors"
#           end

#           lock_screen:
#             timeout 180 # 3 minute(s) after dpms
#             command "${pkgs.swaylock}/bin/swaylock"

#             # Optional per-step notification:
#             # notification "Locking session soon"
#             # notify_seconds_before 15
#           end

#           suspend:
#             timeout 600 # 10 minute(s) after lock
#             command "${pkgs.systemd}/bin/systemctl suspend-then-hibernate"
#           end
#         end

#         # Laptop plan: Battery power (aggressive)
#         battery:
#           brightness:
#             timeout 30 # 1 minute(s)
#             command "${pkgs.brightnessctl}/bin/brightnessctl set 30%"
#           end

#           dpms:
#             timeout 30 # 30 second(s) after brightness
#             command "${pkgs.niri}/bin/niri msg action power-off-monitors"
#             resume_command "${pkgs.niri}/bin/niri msg action power-on-monitors"
#           end

#           lock_screen:
#             timeout 30 # 1 minute(s) after dpms
#             command "${pkgs.swaylock}/bin/swaylock"
#           end

#           suspend:
#             timeout 120 # 2 minute(s) after lock
#             command "${pkgs.systemd}/bin/systemctl suspend-then-hibernate"
#           end
#         end
#       end
# '';
#  };
  home.packages = with pkgs; [
    brightnessctl
    playerctl
    swaybg
    swayidle
    swaylock
    fuzzel
    foot
    waybar
  ];

  # services.swaync.enable = true;
  # services.swayidle = {
  #   enable = true;
  #   # events = [{ before-sleep = "${pkgs.swaylock}/bin/swaylock --daemonize --show-failed-attempts"; }];
  #   # timeouts = [
  #   #   # Timeouts in seconds
  #   #   { timeout = 300; command = "${pkgs.swaylock}/bin/swaylock --show-failed-attempts"; }
  #   #   { timeout = 600; command = "${pkgs.systemd}/bin/systemctl suspend-then-hibernate"; }
  #   # ];
  # };

  # programs.waybar = {
  #   enable = true;
  #   settings = {
  #     mainBar = {
  #       layer = "top";
  #       height = 20;
  #       modules-center = [
  #         "clock"
  #       ];
  #       modules-left = [
  #         "niri/workspaces"
  #       ];
  #       modules-right = [
  #         "cpu"
  #         "memory"
  #         "disk"
  #         "battery"
  #         "idle_inhibitor"
  #         # "niri/window"
  #       ];
  #     };
  #   };
  #   systemd.enable = true;
  # };

  programs.niri.package = pkgs.niri;
  programs.niri.settings = {
    input.keyboard.xkb = {
      options = "ctrl:nocaps,altwin:swap_lalt_lwin,compose:rctrl,shift:both_capslock";
    };
    # environment = {
    #   DISPLAY = ":0";
    # };
    # prefer-no-csd = true;
    spawn-at-startup = [
      # { command = ["xwayland-satellite"]; }
      # { argv = [ "waybar" ]; }
      # { argv = [ "mako" ]; }
      # { command = ["mako"]; }
    ];
    switch-events = {
      lid-close.action.spawn = ["systemctl" "suspend-then-hibernate"];
    };
    clipboard.disable-primary = true;
    window-rules = [
      # Open the Firefox picture-in-picture player as floating by default.
      # This app-id regular expression will work for both:
      # - host Firefox (app-id is "firefox")
      # - Flatpak Firefox (app-id is "org.mozilla.firefox")
      {
        matches = [
          {
            app-id = "firefox$";
            title = "^Picture-in-Picture$";
          }
        ];
        open-floating = true;
      }
    ];
    # layout = {
    #   # If you leave the brackets empty, the windows themselves will decide their initial width.
    #   # default-column-width {}
    #   #
    #   # By default focus ring and border are rendered as a solid background rectangle
    #   # behind windows. That is, they will show up through semitransparent windows.
    #   # This is because windows using client-side decorations can have an arbitrary shape.
    #   #
    #   # If you don't like that, you should uncomment `prefer-no-csd` below.
    #   # Niri will draw focus ring and border *around* windows that agree to omit their
    #   # client-side decorations.
    #   #
    #   # Alternatively, you can override it with a window rule called
    #   # `draw-border-with-background`.
    #   #
    #   # You can change how the focus ring looks.
    #   focus-ring = {
    #     enable = true;
    #     width = 3;
    #     active.color = "#7fc8ff";
    #     # active.gradient = {

    #     # };
    #   };
      # shadow.enable = true;
      # tab-indicator = {
      #   position = "top";
      #   gaps-between-tabs = 10;

      #   hide-when-single-tab = true;
      #   # place-within-column = true;

      #   # active.color = "red";
      # };
    # };
    binds = {
      "Mod+T".action.spawn = "ghostty";
      # "Mod+D".action.spawn = "fuzzel";
      "Mod+O" = {
        action.toggle-overview = [ ];
        repeat = false;
      };
      "Mod+Q" = {
        action.close-window = [ ];
        repeat = false;
      };

      # "Super+Alt+L".action.spawn = "swaylock";

      "Mod+Shift+Slash".action.show-hotkey-overlay = [ ];

      "Print".action.screenshot = [ ];

      "Mod+H".action.focus-column-left = [ ];
      "Mod+L".action.focus-column-right = [ ];
      "Mod+J".action.focus-window-down = [ ];
      "Mod+K".action.focus-window-up = [ ];

      "Mod+WheelScrollRight".action.focus-column-right = [ ];
      "Mod+WheelScrollLeft".action.focus-column-left = [ ];
      "Mod+Ctrl+WheelScrollRight".action.move-column-right = [ ];
      "Mod+Ctrl+WheelScrollLeft".action.move-column-left = [ ];

      "Mod+Shift+H".action.focus-monitor-left = [ ];
      "Mod+Shift+L".action.focus-monitor-right = [ ];

      "Mod+Shift+J".action.focus-workspace-down = [ ];
      "Mod+Shift+K".action.focus-workspace-up = [ ];
      "Mod+Shift+WheelScrollDown" = {
        action.focus-workspace-down = [ ];
        cooldown-ms = 150;
      };
      "Mod+Shift+WheelScrollUp" = {
        action.focus-workspace-up = [ ];
        cooldown-ms = 150;
      };
      "Mod+Ctrl+WheelScrollDown" = {
        action.move-column-to-workspace-down = [ ];
        cooldown-ms = 150;
      };
      "Mod+Ctrl+WheelScrollUp" = {
        action.move-column-to-workspace-up = [ ];
        cooldown-ms = 150;
      };
      "Mod+WheelScrollDown" = {
        action.move-column-to-workspace-down = [ ];
        cooldown-ms = 150;
      };
      "Mod+WheelScrollUp" = {
        action.move-column-to-workspace-up = [ ];
        cooldown-ms = 150;
      };

      "Mod+Shift+Ctrl+J".action.move-column-to-workspace-down = [ ];
      "Mod+Shift+Ctrl+K".action.move-column-to-workspace-up = [ ];

      "Mod+Ctrl+H".action.move-column-left = [ ];
      "Mod+Ctrl+L".action.move-column-right = [ ];
      "Mod+Ctrl+J".action.move-window-down = [ ];
      "Mod+Ctrl+K".action.move-window-up = [ ];

      # The following binds move the focused window in and out of a column.
      # If the window is alone, they will consume it into the nearby column to the side.
      # If the window is already in a column, they will expel it out.
      "Mod+BracketLeft".action.consume-or-expel-window-left = [ ];
      "Mod+BracketRight".action.consume-or-expel-window-right = [ ];

      # Consume one window from the right to the bottom of the focused column.
      # "Mod+Comma".action.consume-window-into-column = [ ];

      # Expel the bottom window from the focused column to the right.
      "Mod+Period".action.expel-window-from-column = [ ];

      # Toggle between preset column width(s)
      "Mod+R".action.switch-preset-column-width = [ ];
      "Mod+Shift+R".action.switch-preset-window-height = [ ];
      "Mod+Ctrl+R".action.reset-window-height = [ ];
      # Expand the focused column to space not taken up by other fully visible columns.
      # Makes the column "fill the rest of the space".
      "Mod+M".action.maximize-column = [ ];
      "Mod+Ctrl+M".action.maximize-window-to-edges = [ ];
      "Mod+F".action.expand-column-to-available-width = [ ];
      "Mod+Shift+F".action.fullscreen-window = [ ];

      "Mod+C".action.center-column = [ ];
      "Mod+Ctrl+C".action.center-visible-columns = [ ];

      # While maximize-column leaves gaps and borders around the window,
      # maximize-window-to-edges doesn't: the window expands to the edges of the screen.
      # This bind corresponds to normal window maximizing,
      # e.g. by double-clicking on the titlebar.
      # "Mod+Shift+M".action.maximize-window-to-edges = [ ];
      # "Mod+Ctrl+F".action.expand-column-to-available-width = [ ];

      "Mod+Minus".action.set-column-width = "-10%";
      "Mod+Equal".action.set-column-width = "+10%";
      "Mod+Shift+Minus".action.set-window-height = "-10%";
      "Mod+Shift+Equal".action.set-window-height = "+10%";

      "Mod+Shift+Escape".action.toggle-keyboard-shortcuts-inhibit = [ ];
      "Mod+Shift+E".action.quit = [ ];
      "Mod+Shift+P".action.power-off-monitors = [ ];

      # Move the focused window between the floating and the tiling layout.
      # "Mod+V".action.toggle-window-floating = [ ];
      "Mod+Shift+V".action.switch-focus-between-floating-and-tiling = [ ];

      "Mod+Shift+Ctrl+T".action.toggle-debug-tint = [ ];

      # Toggle tabbed column display mode.
      # Windows in this column will appear as vertical tabs,
      # rather than stacked on top of each other.
      "Mod+W".action.toggle-column-tabbed-display = [ ];

      # "XF86AudioRaiseVolume" = {
      #   action.spawn-sh = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
      #   allow-when-locked = true;
      # };
      # "XF86AudioLowerVolume" = {
      #   action.spawn-sh = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
      #   allow-when-locked = true;
      # };
      # "XF86AudioMute" = {
      #   action.spawn-sh = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
      #   allow-when-locked = true;
      # };

      # "XF86MonBrightnessUp" = {
      #   action.spawn-sh = "brightnessctl set 10%+";
      #   allow-when-locked = true;
      # };
      # "XF86MonBrightnessDown" = {
      #   action.spawn-sh = "brightnessctl set 10%-";
      #   allow-when-locked = true;
      # };

      "XF86AudioPlay" = {
        action.spawn-sh = "playerctl play-pause";
        allow-when-locked = true;
      };
      "XF86AudioStop" = {
        action.spawn-sh = "playerctl stop";
        allow-when-locked = true;
      };
      "XF86AudioPrev" = {
        action.spawn-sh = "playerctl previous";
        allow-when-locked = true;
      };
      "XF86AudioNext" = {
        action.spawn-sh = "playerctl next";
        allow-when-locked = true;
      };

      # Applications such as remote-desktop clients and software KVM switches may
      # request that niri stops processing the keyboard shortcuts defined here
      # so they may, for example, forward the key presses as-is to a remote machine.
      # It's a good idea to bind an escape hatch to toggle the inhibitor,
      # so a buggy application can't hold your session hostage.
      #
      # The allow-inhibiting=false property can be applied to other binds as well,
      # which ensures niri always processes them, even when an inhibitor is active.
      "Mod+Escape" = {
        action.toggle-keyboard-shortcuts-inhibit = [ ];
        allow-inhibiting = false;
      };
    };
    # xwayland-satellite.path = "${lib.getExe pkgs.xwayland-satellite}";
  };
  # services.mako = {
  #   enable = true;
  # };
}
