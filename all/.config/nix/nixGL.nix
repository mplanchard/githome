# This overlay is copied from https://github.com/guibou/nixGL/issues/16#issuecomment-773316321
# and converted into a function


# this is the nixgl "channel", i.e. the evaluation of default.nix in its
# repository. Pass it in to receive an overlay.
nixgl:

final: prev:

with builtins;

let
  lib = prev.lib;

  nixGL = nixgl { pkgs = final; };
  wrapWithNixGL = wrapper: package:
    let
      getBinFiles = pkg:
        lib.pipe "${lib.getBin pkg}/bin" [
          readDir
          attrNames
          (filter (n: match "^\\..*" n == null))
        ];

      wrapperBin = lib.pipe wrapper [
        getBinFiles
        (filter (n: n == (lib.getName wrapper)))
        head
        (x: "${wrapper}/bin/${x}")
      ];

      binFiles = getBinFiles package;
      wrapBin = name:
        final.writeShellScriptBin name ''
          exec ${wrapperBin} ${package}/bin/${name} "$@"
        '';
    in final.symlinkJoin {
      name = "${package.name}-nixgl";
      paths = (map wrapBin binFiles) ++ [ package ];
    };

  # Constructs the `wrapWithWhatever` attrs. In practice only one of these
  # is defined on my system for some reason.
  wrappers = let replacePrefix = replaceStrings [ "wrapWithNixGL" ] [ "nixGL" ];
  in lib.genAttrs [
    "wrapWithNixGLNvidia"
    "wrapWithNixGLIntel"
    "wrapWithNixGLDefault"
  ] (name: wrapWithNixGL final.${replacePrefix name});
in {
  inherit (nixGL) nixGLNvidia nixGLIntel nixGLDefault;
  inherit wrapWithNixGL;
# the // syntax updates the attrset of the first arg with the attrs in the second
} // wrappers
