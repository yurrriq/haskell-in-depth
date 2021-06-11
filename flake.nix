{
  description = "Working through 'Haskell in Depth'";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
  };

  outputs = { self, flake-utils, nixpkgs }:
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
      };
    } //
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          overlays = [ self.overlay ];
          inherit system;
        };
      in
      {
        apps = {
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
              gitAndTools.pre-commit
              haskellPackages.ormolu
              haskellPackages.pointfree
              nixpkgs-fmt
            ]
          );
        };
      });
}
