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
    pkgs.yosys
    pkgs.nextpnr
    pkgs.trellis
    pkgs.ecpprog
    pkgs.python3
    pkgs.python311Packages.pyserial
    pkgs.plantuml
  ];

  shellHook = ''
    echo "Run a local hoogle server with \"hoogle server --local\""
    source env.sh
  '';
}
