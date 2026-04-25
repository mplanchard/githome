{
  matthew = {inputs, lib, config, pkgs, ...}: {
    home.username = "matthew";
    home.homeDirectory = "/home/matthew";
    home.stateVersion = "25.11";
    home.enableNixpkgsReleaseCheck = true;
    xdg.enable = true;

    home.packages = with pkgs; [
      file
      findutils
      gnumake
      neovim
      ripgrep
      vim
    ];
  };
}
