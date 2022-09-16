{ stdenv
, dpkg
, file
, gcc-unwrapped
, glibc
, lib
, libnl
, openssl
, zlib
}:
let

  # Please keep the version x.y.0.z and do not update to x.y.76.z because the
  # source of the latter disappears much faster.
  version = "6.45.0-14203";

  src = ./falcon-sensor_6.45.0-14203_amd64.deb;

in
stdenv.mkDerivation {
  name = "crowdstrike-falcon-sensor-${version}";

  system = "x86_64-linux";

  inherit src;

  # Required for compilation
  nativeBuildInputs = [
    file
    dpkg
  ];

  # Required at running time
  buildInputs = [
    glibc
    gcc-unwrapped
    libnl
    openssl
    zlib
  ];

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out
    # Unpack the debian archive
    dpkg -x $src $out

    # Everything in the debian archive is under /opt/CrowdStrike. Move it all
    # to just /opt
    mkdir -p $out/opt
    mv $out/opt/CrowdStrike/* $out/opt
  '';

  # Set up RPATH and dynamic linker for all the binaries.
  #
  # We can't use autoPatchElfHook because it doesn't recognize some of the
  # dependencies in the binaries, and I couldn't for the life of me get
  # addAutoPatchElfSearchPath to work, although it's possible that it didn't
  # work for the same reason that `autoPatchElfHook` didn't work, i.e. that
  # it couldn't detect the dependencies in the binaries.
  #
  # Anyway, we just do it manually instead. We use --set-interpreter to set
  # up the main linker, and then --set-rpath to all of the libraries that the
  # binaries need. Maybe this means RPATH is a little bloated for some of them,
  # since we're setting them all to the set of ALL libraries that ANY of them
  # need, but I don't care. This was enough of a pain in the ass as it was.
  postFixup = ''
    echo "doing post-fixup"
    for f in "$out/opt/"*; do
      echo "checking $f"
      echo $(file "$f")
      # Exclude shared object files, symlinks, etc.
      if $(file "$f" | grep "ELF" | grep -q "executable"); then
        echo "patching $f"
        patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$f"
        patchelf --set-rpath ${glibc}/lib:${openssl.out.outPath}/lib:${zlib}/lib:${libnl.out.outPath}/lib "$f"
      fi
    done
  '';

  meta = with lib; {
    description = "Crowdstrike Falcon Sensor";
    homepage = https://www.crowdstrike.com/falcon-platform/;
    license = licenses.unfree;
    maintainers = with stdenv.lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}
