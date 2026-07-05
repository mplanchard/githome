{
  pkgs,
  ...
}:
{
  home.username = "matthew";
  home.homeDirectory = "/home/matthew";
  home.stateVersion = "22.11";
  programs.home-manager.enable = true;
}
