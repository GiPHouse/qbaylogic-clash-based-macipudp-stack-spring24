with import ./nix/nixpkgs.nix {};

stdenv.mkDerivation{
    name = "qbaylogic-clash-based-macipudp-stack-spring24" ;
    src = ./. ;
    installPhase= '' 
        touch $out
    '';
}
