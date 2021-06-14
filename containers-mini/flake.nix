{
  description = "Working through 'Haskell in Depth'";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
  };

  outputs = { self, emacs-overlay, flake-utils, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
    in
    {
      overlay = final: prev: {
        myEmacs = prev.emacsWithPackagesFromUsePackage {
          alwaysEnsure = true;
          config = ./emacs.el;
        };
      };
    } //
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          overlays = [
            emacs-overlay.overlay
            self.overlay
          ];
          inherit system;
        };
      in
      {
        apps = {
          radar = flake-utils.lib.mkApp {
            name = "radar";
            drv = self.defaultPackage.${system};
          };
          vocab-builder = flake-utils.lib.mkApp {
            name = "vocab-builder";
            drv = self.defaultPackage.${system};
          };
        };

        defaultPackage = pkgs.haskellPackages.callCabal2nix "haskell-in-depth" self { };

        devShell = with pkgs; mkShell {
          buildInputs = self.defaultPackage.${system}.env.nativeBuildInputs ++ (
            with pkgs; [
              cabal-install
              cargo
              ghcid
              gitAndTools.pre-commit
              haskell-language-server
              haskellPackages.ormolu
              haskellPackages.pointfree
              myEmacs
              nixpkgs-fmt
            ]
          );
        };
      });
}
