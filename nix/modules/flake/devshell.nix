{ config, inputs, ... }:
{
  imports = [
    inputs.agenix-shell.flakeModules.default
  ];

  agenix-shell = {
    secrets = {
      foo.file = ./secrets/openrouter.key.age;
    };
  };

  defaultShellPackages =
    pkgs: with pkgs; [
      just
      nixd
      ghciwatch
    ];

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
        ];
        packages = config'.defaultShellPackages pkgs;

        shellHook = ''
          source ${lib.getExe config.agenix-shell.installationScript}
        '';
      };
    };
}
