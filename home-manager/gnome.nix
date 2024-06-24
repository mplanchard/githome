{ hmConfig, pkgs, ... }:

hmConfig // {
  home = hmConfig.home or {} // {
    packages = with pkgs; hmConfig . home.packages or [] ++ [
      gnome.dconf-editor
      gnome.gnome-shell-extensions
      gnome.gnome-tweaks
      gnomeExtensions.appindicator
      pavucontrol
    ];
  };

  # Fix for issue with SSH_AUTH_SOCK being overridden.
  # See https://github.com/NixOS/nixpkgs/issues/101616, which links to this
  # now old version of the arch wiki: https://wiki.archlinux.org/index.php?title=GNOME/Keyring&oldid=731014
  #
  # Eventually nixos will switch to using gcr-ssh-agent and we can just disable
  # that service.
  xdg.configFile."autostart/gnome-keyring-ssh.desktop".text = ''
     ${pkgs.lib.fileContents "${pkgs.gnome3.gnome-keyring}/etc/xdg/autostart/gnome-keyring-ssh.desktop"}
     Hidden=true
   '';
}
