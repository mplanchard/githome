{ pkgs, unstable, ... }:

{
  programs = {
    nix-index = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };
  };

  home = {
    packages = with pkgs; [
      unstable._1password-cli
      unstable._1password-gui
      # dropbox
      niv
    ];
  };

}
