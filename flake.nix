{
  description = "Working through 'Haskell in Depth'";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";

  };

  outputs = { self, flake-utils, nixpkgs }:

    flake-utils.lib.eachDefaultSystem (system: {

      devShell = with nixpkgs.legacyPackages.${system}; mkShell {
        buildInputs = [
          cargo
          gitAndTools.pre-commit
          nixpkgs-fmt
        ];
      };

    });

}
