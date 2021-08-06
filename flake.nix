{
  description = "A framework for building tool integrations with Sonatype Lift";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          lift-tools-framework =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8105";
              shell.tools = {
                cabal = {};
                ghcid = {};
                haskell-language-server = {};
                hlint = {};
              };
              shell.withHoogle = true;
              shell.exactDeps = true;
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.lift-tools-framework.flake {
        crossPlatforms = p: [];
      };
    in flake // {
      defaultPackage = flake.packages."lift-tools-framework:lib:lift-tools-framework";
    });
}
