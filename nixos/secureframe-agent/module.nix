{ config, pkgs, ... }:

let
  secureframe-agent = pkgs.callPackage (import ./default.nix) { };
in
{
  config = {
    systemd.services.secureframe-agent = {
      description = "Secureframe Agent";

      wantedBy = ["multi-user.target"];
      after = ["local-fs.target" "network-online.target"];
      conflicts = ["shutdown.target"];
      before = ["shutdown.target"];

      unitConfig.DefaultDependencies = false;

      serviceConfig = {
        ExecStartPre = pkgs.writeShellScript "secureframe-agent-init" ''
          # Copy everything from our built derivation into their expected,
          # hard-coded directory.
          cp -Trf ${secureframe-agent}/opt/orbit /opt/orbit
          chmod -R 0755 /opt/orbit
          chmod -R 0600 /opt/orbit/tuf-metadata.json
        '';
        # Wow baby what a big path you have
        ExecStart = "${secureframe-agent}/opt/orbit/bin/orbit/linux/stable/orbit";
        Type = "exec";
        Restart = "no";
        TimeoutStopSpec = "60s";
        KillMode = "control-group";
        KillSignal = "SIGTERM";
      };
    };

    environment.systemPackages = [
      secureframe-agent
    ];
  };
}
