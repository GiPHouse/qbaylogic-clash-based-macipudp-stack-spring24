{ lib, stdenv, fetchFromGitHub, cmake, boost, python3, eigen, python3Packages
, icestorm, trellis, llvmPackages, enableGui ? false
, wrapQtAppsHook ? null, qtbase ? null, OpenGL ? null }:

let
  boostPython = boost.override {
    python = python3;
    enablePython = true;
  };
in stdenv.mkDerivation rec {
  pname = "nextpnr";
  version = "2024.04.17";

  srcs = [
    (fetchFromGitHub {
      owner = "YosysHQ";
      repo = "nextpnr";
      rev = "d3b53d8e1a36abe835a39847aaeb79e045062b8c";
      hash = "sha256-VJCV1BI099pUu/luRk2UUFQc29Vh2DjUKnElPlhyAKM=";
      name = "nextpnr";
    })
    (fetchFromGitHub {
      owner = "YosysHQ";
      repo = "nextpnr-tests";
      rev = "00c55a9eb9ea2e062b51fe0d64741412b185d95d";
      sha256 = "sha256-83suMftMtnaRFq3T2/I7Uahb11WZlXhwYt6Q/rqi2Yo=";
      name = "nextpnr-tests";
    })
  ];

  sourceRoot = "nextpnr";

  nativeBuildInputs = [ cmake ] ++ (lib.optional enableGui wrapQtAppsHook);
  buildInputs = [ boostPython python3 eigen ]
    ++ (lib.optional enableGui qtbase)
    ++ (lib.optional stdenv.cc.isClang llvmPackages.openmp);

  cmakeFlags = [
    "-DCURRENT_GIT_VERSION=${lib.substring 0 7 (lib.elemAt srcs 0).rev}"
    "-DARCH=ecp5"
    "-DBUILD_TESTS=ON"
    "-DICESTORM_INSTALL_PREFIX=${icestorm}"
    "-DTRELLIS_INSTALL_PREFIX=${trellis}"
    "-DTRELLIS_LIBDIR=${trellis}/lib/trellis"
    "-DUSE_OPENMP=ON"
    # warning: high RAM usage
    "-DSERIALIZE_CHIPDBS=OFF"
    "-DPython3_INCLUDE_DIR=${python3}/include/python${python3.pythonVersion}"
  ] ++ (lib.optional enableGui "-DBUILD_GUI=ON")
    ++ (lib.optional (enableGui && stdenv.isDarwin)
      "-DOPENGL_INCLUDE_DIR=${OpenGL}/Library/Frameworks");

  patchPhase = with builtins; ''
    # use PyPy for icestorm if enabled
    substituteInPlace ./ice40/CMakeLists.txt \
      --replace ''\'''${PYTHON_EXECUTABLE}' '${icestorm.pythonInterp}'
  '';

  preBuild = ''
    ln -s ../nextpnr-tests tests
  '';

  doCheck = true;

  postFixup = lib.optionalString enableGui ''
    wrapQtApp $out/bin/nextpnr-generic
    wrapQtApp $out/bin/nextpnr-ice40
    wrapQtApp $out/bin/nextpnr-ecp5
    wrapQtApp $out/bin/nextpnr-gowin
  '';

  meta = with lib; {
    description = "Place and route tool for FPGAs";
    homepage = "https://github.com/yosyshq/nextpnr";
    license = licenses.isc;
    platforms = platforms.all;
    maintainers = with maintainers; [ thoughtpolice emily ];
  };
}
