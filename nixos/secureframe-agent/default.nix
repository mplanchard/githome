{ stdenv
, autoPatchelfHook
, dpkg
, lib
, tree
}:
let
  version = "1.38.1";
  src = ./secureframe-agent.deb;
  secureframe-agent = stdenv.mkDerivation {
    name = "secureframe-agent-${version}";
    system = "x86_64-linux";

    inherit src;

    # Required for compilation
    nativeBuildInputs = [
      autoPatchelfHook
      dpkg
      # tree
    ];

    # Required at runtime
    buildInputs = [];

    unpackPhase = "true";

    installPhase = ''
      mkdir -p $out
      # Unpack the debian archive
      dpkg -x $src $out

      # tree $out
      # exit 1
    '';

    postFixup = ''
      chmod 0600 $out/opt/orbit/tuf-metadata.json
    '';

    meta = with lib; {
      description = "Secureframe Agent";
      homepage = https://app.secureframe.com/;
      license = licenses.unfree;
      maintainers = with stdenv.lib.maintainers; [ ];
      platforms = [ "x86_64-linux" ];
    };
  };
in
  secureframe-agent
