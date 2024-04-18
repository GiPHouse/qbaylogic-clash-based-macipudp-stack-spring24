{ lib, stdenv, fetchFromGitHub, readline, cmake }:

stdenv.mkDerivation rec {
  pname = "abc-verifier";
  version = "2024.04.17";

  src = fetchFromGitHub {
    owner = "yosyshq";
    repo = "abc";
    rev = "1446b7549f9896514ee6b5264f1624a0299e4016";
    hash = "sha256-4qeKjPV6xtmtB1xLwl53TUYiab8Q399uVRKbcRSf0aY=";
  };

  nativeBuildInputs = [ cmake ];
  buildInputs = [ readline ];

  installPhase = "mkdir -p $out/bin && mv abc $out/bin";

  # needed by yosys
  passthru.rev = src.rev;
}
