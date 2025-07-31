{ hmConfig, pkgs, ... }:

hmConfig // {
  home = hmConfig.home or {} // {
    packages = with pkgs; hmConfig . home.packages or [] ++ [
      (callPackage ../pkgs/nnd.nix { })
      gnupg
      # krita
      # libreoffice
      maestral-gui
      powertop
      tlp # power management
    ];
  };
  # Custom services
  systemd.user.services = hmConfig.systemd.user.services or {} // {
    # dropbox = {
    #   Install = {
    #     WantedBy = [ "graphical-session.target" ];
    #   };
    #   Unit = {
    #     Description = "dropbox";
    #   };
    #   Service = {
    #     ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
    #     ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
    #     KillMode = "control-group"; # upstream recommends process
    #     Restart = "on-failure";
    #     PrivateTmp = true;
    #     ProtectSystem = "full";
    #     Nice = 10;
    #   };
    # };
  };
  services = hmConfig.services or {} // {
    # emacs = { enable = true; };
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 43200;  # 12 hours
      defaultCacheTtlSsh = 43200;
      maxCacheTtl = 43200;
      maxCacheTtlSsh = 43200;
      enableSshSupport = true;
      pinentryPackage = pkgs.pinentry-gnome3;
      # extraConfig = ''
      #   allow-emacs-pinentry
      #   allow-loopback-pinentry
      #   pinentry-program /etc/profiles/per-user/matthew/bin/pinentry
      # '';
      # pinentryFlavor = "gnome3";
    };
  };
}
