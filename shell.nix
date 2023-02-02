{ pkgs ? import ./nix/nixpkgs.nix {} }:
let
  colorlight-eth =
    pkgs.haskellPackages.callCabal2nix "clash-eth"
      (pkgs.nix-gitignore.gitignoreSourcePure [./.gitignore] ./.) { };

in colorlight-eth.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.stylish-haskell
    pkgs.yosys
    pkgs.nextpnr
    pkgs.trellis
    pkgs.ecpprog
  ];
})
