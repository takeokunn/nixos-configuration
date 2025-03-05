{ pkgs }:
with pkgs.vimPlugins;
[
  vim-markdown
  # {
  #   type = "lua";
  #   plugin = orgmode;
  #   config = ''
  #     require('orgmode').setup({
  #       org_agenda_files = '~/ghq/github.com/takeokunn/private/**/*.org',
  #       org_default_notes_file = '~/ghq/github.com/takeokunn/private/memo.org',
  #     })
  #   '';
  # }
]
