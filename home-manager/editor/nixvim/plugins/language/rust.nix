{
  lib,
  pkgs,
  ...
}:

{
  plugins.rustaceanvim = {
    enable = true;
    package = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin (
      (pkgs.neovimUtils.buildNeovimPlugin {
        luaAttr = pkgs.neovim-unwrapped.lua.pkgs.rustaceanvim.overrideAttrs (_: {
          # Skip the Darwin Lua Busted check for rustaceanvim 9.2.2-2.
          # spec/integration/lsp_spec.lua fails while waiting for rust-analyzer initialization.
          # Remove when aarch64-darwin's vimPlugins.rustaceanvim is available from cache.nixos.org; recheck after version bumps.
          doCheck = false;
        });
      }).overrideAttrs
        (_: {
          # Keep require/command checks, but skip the outer Busted wrapper because it reruns the same failing integration test.
          doCheck = true;
          dontBustedCheck = true;
          checkInputs = [ pkgs.vimPlugins.neotest ];
        })
    );
  };
}
