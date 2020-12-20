{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
  my-ghc = haskellPackages.ghcWithPackages (h: [ h.criterion h.vector ]);
in
mkShell {
  buildInputs = [ my-ghc ];
}
