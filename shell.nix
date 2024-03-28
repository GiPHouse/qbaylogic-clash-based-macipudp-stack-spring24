{ pkgs ? import ./nix/nixpkgs.nix {} }:
let
  colorlight-eth =
    pkgs.haskellPackages.callCabal2nix "clash-eth"
      (pkgs.nix-gitignore.gitignoreSourcePure [./.gitignore] ./.) { };

in pkgs.haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [ colorlight-eth ];
  nativeBuildInputs = [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.stylish-haskell
    pkgs.haskellPackages.haskell-language-server
    pkgs.gtkwave
    pkgs.yosys
    pkgs.yosys-ecp5-infer-outreg
    pkgs.nextpnr
    pkgs.trellis
    pkgs.ecpprog
    pkgs.python3
    pkgs.python311Packages.pyserial
    pkgs.plantuml
  ];

  shellHook = ''
    export YOSYS_ECP5_INFER_OUTREG_LIB=${pkgs.yosys-ecp5-infer-outreg}/share/yosys/plugins/lib/ecp5_infer_bram_outreg.so
    echo "Run a local hoogle server with \"hoogle server --local\""
    source env.sh
  '';
}
