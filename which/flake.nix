{
  description = "Minimal Haskell cabal project with splitOn";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems f;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.haskellPackages.ghc
              pkgs.haskellPackages.cabal-install
              pkgs.haskellPackages.hlint
              pkgs.haskellPackages.ormolu
            ];
          };
        }
      );
    };
}

