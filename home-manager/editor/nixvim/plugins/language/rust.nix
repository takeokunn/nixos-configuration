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
          # doCheck keeps require/command checks; dontBustedCheck skips the wrapper's duplicate Busted run.
          doCheck = true;
          dontBustedCheck = true;
          checkInputs = [ pkgs.vimPlugins.neotest ];
        })
    );
  };
}
