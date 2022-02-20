{ hmConfig, pkgs, ... }:

{
  programs = hmConfig . programs or {} // {
    starship = hmConfig . programs.starship or {} // {
      enable = true;
      # Enable once bash is configured by home manager
      enableBashIntegration = true;
      settings = hmConfig . programs.starship.settings or {} // {
        # Allow emacs-libvterm to jump between prompts
        format = "$all\\$\\(vterm_prompt_end\\)";
      };
    };

    firefox = {
      enable = true;
    };

    nix-index = {
      enable = true;
      # Enable once I get bash config into home manager
      enableBashIntegration = true;
      enableZshIntegration = true;
    };
  };

  home.packages = with pkgs; hmConfig . packages or [] ++ [
    _1password
    _1password-gui
    dropbox
    niv
  ];
}
