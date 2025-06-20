{ inputs, ... }:
{
  imports = [
    inputs.agenix-shell.flakeModules.default
  ];

  agenix-shell = {
    secrets = {
      openrouter-key.file = ../../../secrets/openrouter.key.age;
    };
  };

  perSystem =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      devShells.agenix = pkgs.mkShell {
        shellHook = ''
          source ${lib.getExe config.agenix-shell.installationScript}
        '';
      };
    };

}
