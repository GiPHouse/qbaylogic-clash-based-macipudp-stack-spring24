with import ./nix/nixpkgs.nix { };

let shell = (import ./shell.nix) { inherit pkgs; };

in dockerTools.buildImage {
  name = "clash-ethernet-CI-image";
  tag = "latest";
  created = "now";
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ gnumake findutils coreutils bash ]
     ++ shell.nativeBuildInputs
     ++ shell.buildInputs;

    pathsToLink = [ "/bin" ];
  };
  config = {
    Env = [
      "IN_NIX_DOCKER=1"
      "YOSYS_ECP5_INFER_OUTREG_LIB=${pkgs.yosys-ecp5-infer-outreg}/share/yosys/plugins/lib/ecp5_infer_bram_outreg.so"
      "LOCALE_ARCHIVE=${glibcLocales}/lib/locale/locale-archive"
      "LANG=en_US.UTF-8"
      "LANGUAGE=en_US:en"
      "LC_ALL=C.UTF-8"
    ];
  };
}
