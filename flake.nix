{
  description = "Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    agenix-shell = {
      url = "github:aciceri/agenix-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, ... }:
    let
      game = nixpkgs.legacyPackages.x86_64-linux.callPackage ./. { };
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    in
    {
      packages.x86_64-linux.default = game;
      devShells.x86_64-linux.default = pkgs.mkShell {
        inputsFrom = [ game.env ];
        packages = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
          ghcid
          hoogle
          fourmolu
        ];
      };
    };
}
