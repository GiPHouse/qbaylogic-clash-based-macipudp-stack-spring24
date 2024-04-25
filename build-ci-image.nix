with import ./nix/nixpkgs.nix { }; 

let shell = (import ./shell.nix) { inherit pkgs; };

in dockerTools.buildImage {
  name = "clash-ethernet-CI-image";
  tag = "latest";
  created = "now";
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ coreutils bash ]
     ++ shell.buildInputs;

    pathsToLink = [ "/bin" ];
  };
 }
