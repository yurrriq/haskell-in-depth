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
        haskellPackages = prev.haskellPackages.override {
          overrides = hfinal: hprev: {
            formatting = hprev.callHackageDirect
              {
                pkg = "formatting";
                ver = "7.1.2";
                sha256 = "sha256-sbHPbVcLDgymul0x0q2c7ZCej+0IV1P1aqGLt9JpfL0=";
              }
              { };
          };
        };

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
          du = flake-utils.lib.mkApp {
            drv = self.packages.${system}.du;
          };
          radar = flake-utils.lib.mkApp {
            name = "radar";
            drv = self.defaultPackage.${system};
          };
          vocab-builder = flake-utils.lib.mkApp {
            name = "vocab-builder";
            drv = self.defaultPackage.${system};
          };
        };

        defaultPackage = self.packages.${system}.haskell-in-depth;

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

        packages = {
          containers-mini = pkgs.haskellPackages.callCabal2nix "containers-mini" ./containers-mini { };
          du = pkgs.haskellPackages.callCabal2nix "du" ./du { };
          haskell-in-depth = pkgs.haskellPackages.callCabal2nix "haskell-in-depth" self { };
        };
      });
}
