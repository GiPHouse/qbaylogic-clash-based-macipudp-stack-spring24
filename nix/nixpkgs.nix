{ sources ? import ./sources.nix }:

let
  overlay = _: pkgs:

  let
    abc-verifier = pkgs.callPackage ./abc-verifier.nix { };
    yosys = pkgs.callPackage ./yosys.nix {
      abc-verifier = abc-verifier;
    };
    yosys-ecp5-infer-outreg = pkgs.callPackage ./yosys-ecp5-infer-outreg.nix {
      yosys = yosys;
    };
    trellis = pkgs.callPackage ./trellis.nix { };
    nextpnr = pkgs.callPackage ./nextpnr.nix {
      trellis = trellis;
    };
  in {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (pkgs) lib; };

    # Flashing utilitity
    ecpprog = pkgs.callPackage ./ecpprog.nix { };

    # uptodate FPGA tooling
    abc-verifier = abc-verifier;
    yosys = yosys;
    yosys-ecp5-infer-outreg = yosys-ecp5-infer-outreg;
    trellis = trellis;
    nextpnr = nextpnr;

    # Haskell overrides
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        # Add overrides here
        circuit-notation = self.callCabal2nix "circuit-notation" sources.circuit-notation {};
        # dontCheck disables test dependencies which gave problems here
        clash-cores-crc =
          self.callCabal2nix "clash-cores-crc" sources.clash-cores-crc {};
        clash-protocols =
          pkgs.haskell.lib.dontCheck (self.callCabal2nix "clash-protocols" sources.clash-protocols {});
        doctest-parallel =
          self.callCabal2nix "doctest-parallel" sources.doctest-parallel {};
        clash-prelude =
          self.callCabal2nix "clash-prelude" (sources.clash-compiler + "/clash-prelude") {};
        clash-prelude-hedgehog =
          self.callCabal2nix "clash-prelude-hedgehoge" (sources.clash-compiler + "/clash-prelude-hedgehog") {};
        clash-lib =
          self.callCabal2nix "clash-lib" (sources.clash-compiler + "/clash-lib") {};
        clash-ghc =
          self.callCabal2nix "clash-ghc" (sources.clash-compiler + "/clash-ghc") {};
        clash-cores =
          self.callCabal2nix "clash-cores" (sources.clash-compiler + "/clash-cores") {};
        tasty-hedgehog =
          self.callCabal2nix "tasty-hedgehog" sources.tasty-hedgehog {};
        hedgehog =
          self.callCabal2nix "hedgehog" (sources.haskell-hedgehog + "/hedgehog") {};
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
