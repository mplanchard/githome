{
  pkgs,
  ...
}:
let
  sshPort = 2223;
  wireguardEndpoint = "89.187.178.173";
  wireguardPort = 51820;
in
{
  networking.hostName = "smol"; # Define your hostname.
  networking.firewall.enable = true;

  # Wireguard VPN config to route all traffic.
  # Mostly taken from https://wiki.nixos.org/wiki/WireGuard

  networking.networkmanager.enable = true;
  # Enable network manager applet
  programs.nm-applet.enable = true;

  networking.useNetworkd = true; # use systemd-networkd
  networking.firewall.allowedUDPPorts = [ wireguardPort ]; # wireguard

  # NixOS firewall will block wg traffic because of rpfilter
  networking.firewall.checkReversePath = "loose";
  systemd.network = {
    enable = true;
    # Wireguard device
    netdevs."50-wg0" = {
      netdevConfig = {
        Kind = "wireguard";
        Name = "wg0";
      };

      wireguardConfig = {
        ListenPort = wireguardPort;
        # ensure file is readable by `systemd-network` user
        PrivateKeyFile = "/etc/wg.key";
        # To automatically create routes for everything in AllowedIPs,
        # add RouteTable=main
        RouteTable = "main";

        # FirewallMark marks all packets send and received by wg0
        # with the number 42, which can be used to define policy rules on these packets.
        FirewallMark = 42;
      };
      wireguardPeers = [
        {
          PublicKey = "/KM6QESKJRK7GiMqWstUl1Yn9pzc6DPzqCtNauxYgn8=";
          AllowedIPs = [
            # proxy all traffic
            "0.0.0.0/0"
            "::/0"
          ];
          # can't use domain
          # Routing all DNS over WireGuard (i.e. Domains=~.) will prevent the DNS resolution of endpoints.
          Endpoint = "${wireguardEndpoint}:${toString (wireguardPort)}";

          # RouteTable line specifies that a new routing table with id 1000 is created
          # for the wireguard interface, and no rules are set on the main routing table.
          RouteTable = 1000;
        }
      ];
    };
    # Wireguard network
    networks."50-wg0" = {
      matchConfig.Name = "wg0";
      # /32 and /128 specifies a single address for use on this wg peer machine
      address = [
        "10.2.0.2/32"
        "2a07:b944::2:2/128"
      ];
      routingPolicyRules = [
        # rule 1: redirect traffic
        {
          Family = "both"; # ipv4 & 6
          # For all packets *not* marked with 42 (i.e. all non-wireguard/normal traffic),
          InvertRule = true;
          FirewallMark = 42;

          # (... continued) we specify that the routing table 1000 must be used
          # (which is the wireguard routing table). This rule routes all traffic through wireguard.
          # inside routingPolicyRules section is called Table, not RouteTable
          Table = 1000;

          # this routing policy rule has a lower priority (10) than
          # endpoint exclusion rule (5).
          Priority = 10;
        }
        # rule 2: exclude endpoint ip
        {
          # Use a routing policy rule to exclude the endpoint IP address,
          # so that wireguard can still connect to it.
          # it has a higher priority (5) than (10).

          # We exempt our endpoint with a higher priority by routing it
          # through the main table (Table=main is default).
          To = "${wireguardEndpoint}/32";
          Priority = 5;
        }
        # rule 3: exclude inbound ssh
        {
          # This allows SSH connections into the machine.
          SourcePort = sshPort;
          Priority = 3;
          Table = "main";
        }
      ];
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    ports = [ sshPort ];
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
      AllowUsers = [ "matthew" ];
    };
  };
  services.fail2ban.enable = true;
  users.users.matthew.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCw9k/MbnhjNCv5xDyP810yiMQZyIyppuFq1SnROj2mYNx7onrOAULc12YeT0MZfoLuaZAGN/BiEZHp41XBmfH+q7lsH1dG2jHNNYxUdAzezLpepkxJsWQGCgjvFWbQiF5WYI0bz/OFs+aaa7bUMjXrYbEiaVMAy+ZJYPqnBywaFKk2PwMJEYF/I+OUoDQi2MRPRXkg4PnCL9OT6tFLI+ljRiaCTDEK5ksuzjX+L5ReBjNFB0EvOtWEDV6N07LO9mFe4wOgCngs7gCFbyT+TqY9IYqnWFlntRMtVrlnIDPj//ngdopLp7Mod3zBRTjAKlvZUa4Y7nXbZgU1yQIDOcUc3KaH87XU0K8ESmLOGWD34Poe9yNODkt4Z97uINPB1UWwbSvbt3yoItVHU8KCK6QdlhKvnPvOHGb1JoKbUOObLgFQ6gTSTZhzxy9EkWSIfu5+gafSvmkEVMllQStmZI8Eepo7bEyY8pSzYn2U6pz+nUyLAb9ZF0qqxfNpg/iEowVJhDzg/VHuKns96F0nOz66EiA91GMyoZQrTFSXQlQnxLZEoaMmj1zlWwIbyQueXiHqzEUd3GNqPkK80ATGYcDZL8gGhqQXWK2r3swPAUQwlNwoLny9Ohh7HWbaLKyVjY3A8kTvsdztIpfOePq+LCRXTyRTcMQ5uPQxwXjPS0102Q== (none)"
  ];
}
