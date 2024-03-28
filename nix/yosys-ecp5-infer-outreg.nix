{ stdenv, lib, fetchFromGitHub, yosys, readline, zlib }:

stdenv.mkDerivation rec {
  pname = "yosys-ecp5-infer-outreg";
  version = "0.1";

  # Use custom version for bram output register inference
  src = fetchFromGitHub {
    owner = "rowanG077";
    repo = "yosys_ecp5_infer_bram_outreg";
    rev = "7219ee2dd5ed7de7c5167e58a200d3243f792281";
    hash = "sha256-OICYk2deXQjjS1QasRMvYVHoV9JfFyesmliIf5HAJCk=";
  };

  buildInputs = [ yosys readline zlib ];

  makeFlags = [ "PREFIX=$(out)/share/yosys/plugins" ];

  meta = with lib; {
    description = "Yosys plugin that allows inference of output registers on the ECP5 DP16KD memory primitive.";
    homepage = "https://github.com/rowanG077/yosys_ecp5_infer_bram_outreg";
    license = licenses.mit;
    platforms = platforms.all;
    maintainers = with maintainers; [ rowanG077 ];
  };
}
