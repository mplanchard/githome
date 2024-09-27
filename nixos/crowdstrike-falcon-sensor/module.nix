{ config, lib, pkgs, ... }:

let
  crowdstrike-falcon-sensor = (import ./default.nix) { inherit pkgs; };
  # crowdstrike-falcon-sensor = pkgs.buildFHSUserEnv {
  #   name = "crowdstrike-falcon-sensor-env";
  #   targetPkgs = pkgs: [ crowdstrike-falcon-sensor-pkg ];
  # };
in
{
  config = {
    # Crowdstrike hard-codes paths to this directory, so make sure it exists.
    # Allow anyone to read/write to it b/c idgaf.
    systemd.tmpfiles.rules = [
      "d '/opt/CrowdStrike' 0777 - - - -"
    ];

    systemd.services.crowdstrike-falcon-sensor = {
      description = "CrowdStrike Falcon Sensor";

      wantedBy = ["multi-user.target"];
      after = ["local-fs.target"];
      conflicts = ["shutdown.target"];
      before = ["shutdown.target"];

      unitConfig.DefaultDependencies = false;

      # this doesn't actually work (wound up having to get the SSL linking
      # going through RPATH), but now that I have it working I am afraid to
      # remove it.
      environment = {
        OPENSSL_DIR = pkgs.openssl.dev.outPath;
        OPENSSL_LIB_DIR = "${pkgs.openssl.out.outPath}/lib";
      };

      serviceConfig = {
        ExecStartPre = pkgs.writeShellScript "falcon-init" ''
          # For some reason the falcon-sensor.log that they themselves
          # create they don't have write permission to, so we help them along.
          touch /var/log/falcon-sensor.log
          chmod 0666 /var/log/falcon-sensor.log

          # Copy everything from our built derivation into their expected,
          # hard-coded directory.
          ln -sf ${crowdstrike-falcon-sensor}/opt/* /opt/CrowdStrike/

          # This is the actual pre-start thing. I guess it reads a config
          # file that they store in /opt/CrowdStrike/falconstore
          ${crowdstrike-falcon-sensor}/opt/falconctl -f -s --cid=38E9E0099F8643ECB3DB133113DE8B0D-47

          # Again, even though they're creating this shit, subsequent programs
          # weren't able to access, so let anybody read the store.
          chmod 0666 /opt/CrowdStrike/falconstore
        '';
        # This runs the little daemon
        ExecStart = "${crowdstrike-falcon-sensor}/opt/falcond";
        Type = "forking";
        PIDFile = "/run/falcond.pid";
        Restart = "no";
        TimeoutStopSpec = "60s";
        KillMode = "control-group";
        KillSignal = "SIGTERM";
      };
    };

    environment.systemPackages = [
      crowdstrike-falcon-sensor
    ];
  };
}
