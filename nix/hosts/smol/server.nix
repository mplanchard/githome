{
  pkgs,
  config,
  ...
}:
let
  rootHtml = pkgs.writeTextDir "www/index.html" ''
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="smol" content="The Local SmolNet">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Smol Index</title>
</head>
<body>
  <p>
    <a href="http://smol.local:9091/">Torrents</a>
    <br/>
    <a href="http://smol.local:32400/web">Plex</a>
  </p>
</body>
</html>
'';
in
{
  networking.firewall.allowedTCPPorts = [ 80 ];
  services.static-web-server = {
    enable = true;
    root = "${rootHtml}/www";
    listen = "[::]:80";
  };
}
