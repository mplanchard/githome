{ pkgs, unstable, ... }:

{
  home = {
    packages = with pkgs; [
      # (callPackage ../pkgs/nnd.nix { })
      appimage-run
      dmidecode
      gnupg
      # krita
      # libreoffice
      libinput-gestures
      maestral-gui
      powertop
      tlp # power management
      unstable.signal-desktop
      xdg-utils
      xorg.xev
      zulip
    ];
  };
  # Custom services
  # systemd.user.services = hmConfig.systemd.user.services or {} // {
  #   # dropbox = {
  #   #   Install = {
  #   #     WantedBy = [ "graphical-session.target" ];
  #   #   };
  #   #   Unit = {
  #   #     Description = "dropbox";
  #   #   };
  #   #   Service = {
  #   #     ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
  #   #     ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
  #   #     KillMode = "control-group"; # upstream recommends process
  #   #     Restart = "on-failure";
  #   #     PrivateTmp = true;
  #   #     ProtectSystem = "full";
  #   #     Nice = 10;
  #   #   };
  #   # };
  # };

  services = {
    # dropbox.enable = true;
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 43200;  # 12 hours
      defaultCacheTtlSsh = 43200;
      maxCacheTtl = 43200;
      maxCacheTtlSsh = 43200;
      enableSshSupport = true;
      pinentry.package = pkgs.pinentry-gnome3;
      # extraConfig = ''
      #   allow-emacs-pinentry
      #   allow-loopback-pinentry
      #   pinentry-program /etc/profiles/per-user/matthew/bin/pinentry
      # '';
      # pinentryFlavor = "gnome3";
    };
  };
}
