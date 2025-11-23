{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          devShell = {
            # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
            # Check that haskell-language-server works
            hlsCheck.enable = true; # Requires sandbox to be disabled
          };
        };

        packages.default = self'.packages.groq-hs;
      };
    };
}
