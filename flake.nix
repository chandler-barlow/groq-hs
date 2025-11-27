{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ 
        inputs.haskell-flake.flakeModule 
        inputs.treefmt-nix.flakeModule
      ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc912;
          devShell = {
            tools = hp: { 
              fourmolu = hp.fourmolu; 
              ghcid = null; 
              haskell-language-server = hp.haskell-language-server;
            };
            # Check that haskell-language-server works
            hlsCheck.enable = true; # Requires sandbox to be disabled
          };
        };

        packages.default = self'.packages.groq-hs;

        treefmt.config = {
            # inherit (config.flake-root) projectRootFile;
            package = pkgs.treefmt;
            flakeFormatter = false; # For https://github.com/numtide/treefmt-nix/issues/55

            programs.ormolu.enable = true;
            programs.nixpkgs-fmt.enable = true;
            programs.cabal-fmt.enable = true;
            programs.hlint.enable = true;

            programs.ormolu.package = pkgs.haskellPackages.fourmolu;
            settings.formatter.ormolu = {
              options = [
                "--ghc-opt"
                "-XImportQualifiedPost"
              ];
            };
        };
      };

    };
}
