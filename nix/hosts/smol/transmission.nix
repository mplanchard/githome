{
  pkgs,
  config,
  ...
}:
let
  transmissionPort = 9091;
in
{
  services.transmission = {
    enable = true;
    package = pkgs.transmission_4;
    group = "media";
    openRPCPort = true;
    openPeerPorts = true;
    settings = {
      anti_brute_force_enabled = true;
      anti_brute_force_threshold = 100;

      rpc_authentication_required = true;
      rpc_bind_address = "0.0.0.0";
      rpc_host_whitelist_enabled = false;
      rpc_password = "{dae452967104fb9c5970f448f9e48c4412f0306aZaGudr4X";
      rpc_port = transmissionPort;
      rpc_whitelist = "192.168.1.*,192.168.12.*";

      speed_limit_up_enabled = true;
      speed_limit_up = 100;  # kb/s
    };
  };
}
