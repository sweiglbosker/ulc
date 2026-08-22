{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPkgs = pkgs.ocamlPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with ocamlPkgs; [
            ocaml
            dune_3
            utop
            findlib
            ocaml-lsp
            ocamlformat
          ];
        };
      }
    );
}
