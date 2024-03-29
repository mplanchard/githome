{ hmConfig, pkgs, ... }:

hmConfig // {
  home = hmConfig.home or {} // {
    packages = with pkgs; hmConfig . home.packages or [] ++ [
        kdeconnect
        korganizer
        obs-studio
        obs-studio-plugins.wlrobs
        yakuake
        plasma5Packages.accounts-qt
        plasma5Packages.akonadi
        plasma5Packages.akonadi-calendar
        plasma5Packages.calendarsupport
        plasma5Packages.kaccounts-integration
        plasma5Packages.kaccounts-providers
        plasma5Packages.kcalendarcore
        plasma5Packages.kcharselect
        plasma5Packages.kontact
    ];
  };
}
