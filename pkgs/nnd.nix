{ pkgs, lib, pkgsStatic }:

pkgsStatic.rustPlatform.buildRustPackage rec {
  pname = "nnd";
  version = "v0.12";
  src = pkgs.fetchFromGitHub {
    owner = "al13n321";
    repo = "nnd";
    rev = version;
    hash = "sha256-Xoosn8gegI3M1+xMR57AHZEk/qAGnHFqqFcJkksOcsQ=";
  };

  nativeBuildInputs = [ pkgsStatic.musl.dev ];
  buildType = "dbgo";

  cargoHash = "sha256-x5YmGEzGOZqV0peaqlfEGMwf6aVAJ4JymduIL5Cgiw4=";

  RUSTFLAGS = "-Cforce-unwind-tables=yes -Cforce-frame-pointers=yes -Ctarget-feature=+avx2";

  meta = {
    description = "A debugger for Linux. Partially inspired by RemedyBG.";
    homepage = "https://github.com/al13n321/nnd";
    license = lib.licenses.asl20;
    maintainers = [ ];
  };
}
