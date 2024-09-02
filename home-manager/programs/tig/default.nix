{ pkgs }: {
  home.file = {
    ".tig/dracula/".source = builtins.fetchGit {
      url = "https://github.com/dracula/tig";
      rev = "e8a3387d8353e90cca41f5d89c3e1f74f1f7c8c6";
    };
  };

  programs.tig = {
    enable = true;
    config = ''
      # config

      set main-view = id date author:email-user commit-title:graph=yes,refs=yes
      set blame-view = date:default author:email-user id:yes,color line-number:yes,interval=1 text

      set log-view = line-number:yes,interval=1 text
      set blob-view = line-number:yes,interval=1 text
      set diff-view = line-number:yes,interval=1 text:yes,commit-title-overflow=no
      set pager-view = line-number:yes,interval=1 text
      set stage-view = line-number:yes,interval=1 text

      set blob-view-line-number = yes
      set blame-view-line-number = yes

      set mouse = true
      set tab-size = 4
      set ignore-case = true
      set refresh-mode = auto
      set ignore-space = at-eol
      set line-graphics = utf-8
      set diff-options = -m --first-parent
      set diff-highlight = true

      # keybind

      bind generic g move-first-line
      bind generic E view-grep
      bind generic G move-last-line

      bind main G move-last-line
      bind main <Esc>g :toggle commit-title-graph

      bind generic <Ctrl-v> move-page-down
      bind generic <Esc>v   move-page-up
      bind generic <Ctrl-g> refresh

      bind generic <Esc>f :toggle file-name
      bind main <Esc>f :toggle commit-title-refs

      bind status P !git push origin
      bind branch L !git pull origin %(branch)

      # plugins
      source ~/.tig/dracula/config
    '';
  };
}
