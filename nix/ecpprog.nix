{ stdenv
, lib
, fetchFromGitHub
, libftdi1
, pkg-config
}:

stdenv.mkDerivation rec {
  pname   = "ecpprog";
  version = "14-9-2022";

  raw_src = fetchFromGitHub {
    owner = "gregdavill";
    repo  = "ecpprog";
    rev   = "8af8863855599f4b8ef8f46a336408b1aba60e9d";
    hash  = "sha256-0SF501IzVQcxQV7BynVByUl3KNeIozGoJ33c+cd+9Eg=";
  };

  src = "${raw_src}/ecpprog";

  installPhase = ''
    mkdir -p $out/bin
    mv ecpprog $out/bin/ecpprog
  '';

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ libftdi1 ];

  meta = with lib; {
    description = "A basic driver for FTDI based JTAG probes to program Lattice ECP5/Nexus FPGAs.";
    homepage    = "https://github.com/gregdavill/ecpprog";
    license     = licenses.isc;
    platforms   = platforms.all;
    maintainers = with maintainers; [ rowanG077 ];
  };
}
