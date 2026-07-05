{ pkgs, ... }:
{
  home.username = "matthew";
  home.homeDirectory = "/home/matthew";

  # ensure home-manager version matches nixpkgs version
  home.enableNixpkgsReleaseCheck = true;
  home.stateVersion = "22.11";

  # Extra variables to add to PATH
  home.sessionPath = [
    "$HOME/bin"
  ];
  home.packages = with pkgs; [
    neovim
  ];

  programs = {
    home-manager.enable = true;
    info.enable = true;
  };

  # Enable XDG env vars for config locations and such
  xdg.enable = true;
}
