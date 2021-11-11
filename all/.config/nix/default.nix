{ config, lib, pkgs, ... }:

pkgs.buildEnv {
  name = "matthew-packages";
  buildInputs = [ pkgs.cowsay ];
}
