{
  description = "Working through 'Haskell in Depth'";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in

      {
        defaultPackage = pkgs.haskellPackages.callCabal2nix "haskell-in-depth" self { };

        devShell = with pkgs; mkShell {
          buildInputs = self.defaultPackage.${system}.env.nativeBuildInputs ++ (
            with pkgs; [
              cabal-install
              cargo
              gitAndTools.pre-commit
              haskellPackages.ormolu
              haskellPackages.pointfree
              nixpkgs-fmt
            ]
          );
        };
      });
}
