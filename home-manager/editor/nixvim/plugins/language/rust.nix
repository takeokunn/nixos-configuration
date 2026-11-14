{ pkgs, ... }:

{
  plugins.rustaceanvim = {
    enable = true;
    package = (pkgs.neovimUtils.buildNeovimPlugin {
      luaAttr = pkgs.neovim-unwrapped.lua.pkgs.rustaceanvim.overrideAttrs (_: {
        # The integration test times out waiting for rust-analyzer on origin/main; remove once fixed upstream.
        doCheck = false;
      });
    }).overrideAttrs (_: {
      checkInputs = [ pkgs.vimPlugins.neotest ];
    });
  };
}
