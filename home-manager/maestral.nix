{ pkgs, ... }:
{
  packages = with pkgs; [
    maestral
    maestral-gui
  ];
  systemd.user.services = {
    maestral = {
      Install = {
        WantedBy = ["default.target"];
      };
      Unit = {
        Description = "maestral";
      };
      Service = {
        Type = "notify";
        NotifyAccess = "exec";
        ExecStart = "${pkgs.maestral}/bin/maestral start -f";
        ExecStop = "${pkgs.maestral}/bin/maestral stop";
        WatchDogSec = "30s";
      };
    };
  };
}
