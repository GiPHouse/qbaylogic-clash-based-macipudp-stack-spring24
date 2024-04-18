{ lib, stdenv, fetchFromGitHub, python3, boost, cmake }:

let
  rev = "2dab0095e1a5691855b0955b329cb4946b6a13b8";
  # git describe --tags
  realVersion = "1.2.1-26-g${builtins.substring 0 7 rev}";
in stdenv.mkDerivation rec {
  pname = "trellis";
  version = "2024-03-28";

  srcs = [
    (fetchFromGitHub {
      owner = "YosysHQ";
      repo = "prjtrellis";
      inherit rev;
      hash = "sha256-miclTStd+ICpPb/3PmZo7QExWPCt9QjIP+U/94LebyI=";
      name = "trellis";
    })

    (fetchFromGitHub {
      owner = "YosysHQ";
      repo = "prjtrellis-db";
      rev = "4dda149b9e4f1753ebc8b011ece2fe794be1281a";
      sha256 = "sha256-mlHjoVB1yWO3TlNeYSxiYQUIrvYGhg1fSKyhHD1L938=";
      name = "trellis-database";
    })
  ];
  sourceRoot = "trellis";

  buildInputs = [ boost ];
  nativeBuildInputs = [ cmake python3 ];
  cmakeFlags = [
    "-DCURRENT_GIT_VERSION=${realVersion}"
    # TODO: should this be in stdenv instead?
    "-DCMAKE_INSTALL_DATADIR=${placeholder "out"}/share"
  ];

  preConfigure = ''
    rmdir database && ln -sfv ${builtins.elemAt srcs 1} ./database
    cd libtrellis
  '';

  postInstall = lib.optionalString stdenv.isDarwin ''
    for f in $out/bin/* ; do
      install_name_tool -change "$out/lib/libtrellis.dylib" "$out/lib/trellis/libtrellis.dylib" "$f"
    done
  '';

  doInstallCheck = true;

  installCheckPhase = ''
    $out/bin/ecppack $out/share/trellis/misc/basecfgs/empty_lfe5u-85f.config /tmp/test.bin
  '';

  meta = with lib; {
    description = "Documentation and bitstream tools for Lattice ECP5 FPGAs";
    longDescription = ''
      Project Trellis documents the Lattice ECP5 architecture
      to enable development of open-source tools. Its goal is
      to provide sufficient information to develop a free and
      open Verilog to bitstream toolchain for these devices.
    '';
    homepage = "https://github.com/YosysHQ/prjtrellis";
    license = licenses.isc;
    maintainers = with maintainers; [ q3k thoughtpolice emily rowanG077 ];
    platforms = platforms.all;
  };
}
