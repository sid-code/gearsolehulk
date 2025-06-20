{
  config,
  lib,
  pkgs,
  ...
}:
{
  options = {
    defaultShellPackages = lib.mkOption {
      type = lib.types.functionTo (lib.types.listOf lib.types.package);
    };
  };
}
