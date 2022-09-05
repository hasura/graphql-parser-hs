# To automatically load this flake into your shell:
#   1. Set up direnv and nix-direnv.
#   2. Create a file named ".envrc.local", with the contents `use flake`.

{
  description = "Hasura GraphQL Engine";

  inputs = {
    nixpkgs = {
      url = github:NixOS/nixpkgs/nixos-22.05;
    };

    flake-utils = {
      url = github:numtide/flake-utils;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    let
      ghcVersion = "8.10.7";
      ghcName = "ghc${builtins.replaceStrings ["."] [""] ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      haskellCompiler = pkgs.haskell.compiler."${ghcName}";
      haskellPackages = pkgs.haskell.packages."${ghcName}";
    in
    {
      packages.default = (haskellPackages.callCabal2nix "graphql-parser" ./. { }).overrideScope (
        self: super: {
          hedgehog = self.hedgehog_1_1_1;
        }
      );

      pkgs.formatter = nixpkgs.legacyPackages."${system}".nixpkgs-fmt;

      devShells.default = pkgs.mkShell {
        name = "graphql-parser-hs";

        # We list top-level packages before packages scoped to the GHC version, so
        # that they appear first in the PATH. Otherwise we might end up with older
        # versions of transitive dependencies (e.g. HLS depending on Ormolu).
        buildInputs = [
          pkgs.cabal-install
          pkgs.hlint
          pkgs.ormolu
          haskellCompiler
          haskellPackages.ghcid
          haskellPackages.haskell-language-server
        ];
      };
    }
    );
}
