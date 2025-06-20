{ config, inputs, ... }:
{
  perSystem =
    let
      config' = config;
    in
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      # Default shell.
      devShells.default = pkgs.mkShell {
        name = "haskell-template";
        meta.description = "Haskell development environment";
        # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
        inputsFrom = [
          config.haskellProjects.default.outputs.devShell # See ./nix/modules/haskell.nix
          config.pre-commit.devShell # See ./nix/modules/formatter.nix
          config.devShells.aider
          config.devShells.agenix
        ];

        packages = with pkgs; [
          just
          nixd
          ghciwatch
        ];
      };
    };
}
