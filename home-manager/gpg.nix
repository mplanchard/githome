{pkgs, ...}:
{
  home.packages = with pkgs; [
      gnupg
      pinentry-all
  ];
  services = {
    # dropbox.enable = true;
    gpg-agent = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
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
