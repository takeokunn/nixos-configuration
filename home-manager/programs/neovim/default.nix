{ pkgs, sources }:
{
  programs.neovim = {
    enable = true;
    withNodeJs = false;
    withRuby = false;
    withPython3 = false;
    plugins = import ./plugins { inherit pkgs sources; };

    extraLuaConfig = builtins.readFile ./init.lua;
  };

  programs.fish = {
    interactiveShellInit = ''
      set -x PAGER "nvim +Man!"
      set -x MANPAGER "nvim +Man!"
    '';
  };
}
