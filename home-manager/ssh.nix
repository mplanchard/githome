{ pkgs, ... }:
{
  home.packages = with pkgs; [ openssh ];
  programs.ssh = {
      enableDefaultConfig = true;
      enable = true;
      # serverAliveInterval = 60;
      matchBlocks = {
        "gitlab" = {
          host = "gitlab.com";
          hostname = "altssh.gitlab.com";
          user = "git";
          port = 443;
        };
        "smol" = {
          host = "smol.local";
          user = "matthew";
          port = 2223;
        };
      };
  };
}
