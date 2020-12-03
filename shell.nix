{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
  my-ghc = haskellPackages.ghcWithPackages (h: [ h.criterion ]);
in
mkShell {
  buildInputs = [ my-ghc ];
}
