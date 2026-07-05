{ pkgs, ... }:
{
  # ensure nix programs can find nix-installed fonts
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    fontconfig
    nerd-fonts.code-new-roman
    nerd-fonts.comic-shanns-mono
    nerd-fonts.envy-code-r
    nerd-fonts.hack
    nerd-fonts.jetbrains-mono
    nerd-fonts.monofur
    nerd-fonts.monoid
    nerd-fonts.mononoki
    nerd-fonts.ubuntu-mono
    nerd-fonts.ubuntu-sans
    nerd-fonts.recursive-mono
    nerd-fonts.sauce-code-pro
    nerd-fonts.space-mono
    nerd-fonts.tinos
    nerd-fonts.zed-mono
  ];
}
