{
  perSystem =
    {
      self',
      pkgs,
      ...
    }:
    {
      devShells.aider = pkgs.mkShell {
        packages = [
          pkgs.aider-chat
        ];
      };
    };
}
