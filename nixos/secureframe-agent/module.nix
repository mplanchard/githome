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
          # This bad boy is hard-coded to want to run out of /opt,
          # so give it a directory and symlink its shit in there
          mkdir /opt/orbit

          # Copy everything from our built derivation into their expected,
          # hard-coded directory.
          ln -sf ${secureframe-agent}/opt/orbit /opt/orbit/

          # EXCEPT this piece of shit, which it insists must have permissions
          # 0600, but being on a read-only filesystem as 0444 is NOT ACCEPTABLE,
          # despite the fact that you can read the thing in their stupid
          # binary package anyway.
          # We can't change perms on a symlink, so just copy the real
          # file in, who cares.
          rm /opt/orbit/tuf-metadata.json
          cp ${secureframe-agent}/opt/orbit/tuf-metadata.json /opt/orbit/tuf-metadata.json
          chmod 0600 /opt/orbit/tuf-metadata.json
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
