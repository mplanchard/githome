{
  pkgs,
  config,
  ...
}:
{
  services.plex = {
    enable = true;
    openFirewall = true;
    group = "media";
  };
}
