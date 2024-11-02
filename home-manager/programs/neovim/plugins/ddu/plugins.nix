{ vimUtils, sources }:
{
  ddu = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu.pname;
    version = sources.vim-ddu.date;
    src = sources.vim-ddu.src;
  };
  ddu-ui-ff = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-ui-ff.pname;
    version = sources.vim-ddu-ui-ff.date;
    src = sources.vim-ddu-ui-ff.src;
  };
  ddu-ui-filer = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-ui-filer.pname;
    version = sources.vim-ddu-ui-filer.date;
    src = sources.vim-ddu-ui-filer.src;
  };
  ddu-kind-file = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-kind-file.pname;
    version = sources.vim-ddu-kind-file.date;
    src = sources.vim-ddu-kind-file.src;
  };
  ddu-kind-word = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-kind-word.pname;
    version = sources.vim-ddu-kind-word.date;
    src = sources.vim-ddu-kind-word.src;
  };
  ddu-source-file = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-file.pname;
    version = sources.vim-ddu-source-file.date;
    src = sources.vim-ddu-source-file.src;
  };
  ddu-source-file_rec = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-file_rec.pname;
    version = sources.vim-ddu-source-file_rec.date;
    src = sources.vim-ddu-source-file_rec.src;
  };
  ddu-source-line = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-line.pname;
    version = sources.vim-ddu-source-line.date;
    src = sources.vim-ddu-source-line.src;
  };
  ddu-filter-matcher_substring = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-matcher_substring.pname;
    version = sources.vim-ddu-filter-matcher_substring.date;
    src = sources.vim-ddu-filter-matcher_substring.src;
  };
  ddu-filter-matcher_hidden = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-matcher_hidden.pname;
    version = sources.vim-ddu-filter-matcher_hidden.date;
    src = sources.vim-ddu-filter-matcher_hidden.src;
  };
  ddu-filter-matcher_relative = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-matcher_relative.pname;
    version = sources.vim-ddu-filter-matcher_relative.date;
    src = sources.vim-ddu-filter-matcher_relative.src;
  };
  ddu-source-path_history = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-path_history.pname;
    version = sources.vim-ddu-source-path_history.date;
    src = sources.vim-ddu-source-path_history.src;
  };
  ddu-source-action = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-action.pname;
    version = sources.vim-ddu-source-action.date;
    src = sources.vim-ddu-source-action.src;
  };
  ddu-source-register = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-register.pname;
    version = sources.vim-ddu-source-register.date;
    src = sources.vim-ddu-source-register.src;
  };
  ddu-source-dummy = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-dummy.pname;
    version = sources.vim-ddu-source-dummy.date;
    src = sources.vim-ddu-source-dummy.src;
  };
  ddu-source-ghq = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-ghq.pname;
    version = sources.vim-ddu-source-ghq.date;
    src = sources.vim-ddu-source-ghq.src;
  };
  readme-viewer = vimUtils.buildVimPlugin {
    pname = sources.vim-readme-viewer.pname;
    version = sources.vim-readme-viewer.date;
    src = sources.vim-readme-viewer.src;
  };
  ddu-filter-kensaku = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-kensaku.pname;
    version = sources.vim-ddu-filter-kensaku.date;
    src = sources.vim-ddu-filter-kensaku.src;
  };
  ddu-filter-merge = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-merge.pname;
    version = sources.vim-ddu-filter-merge.date;
    src = sources.vim-ddu-filter-merge.src;
  };
  ddu-filter-converter_relativepath = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-converter_relativepath.pname;
    version = sources.vim-ddu-filter-converter_relativepath.date;
    src = sources.vim-ddu-filter-converter_relativepath.src;
  };
  ddu-filter-fuse = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-fuse.pname;
    version = sources.vim-ddu-filter-fuse.date;
    src = sources.vim-ddu-filter-fuse.src;
  };
  ddu-source-git_diff = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-git_diff.pname;
    version = sources.vim-ddu-source-git_diff.date;
    src = sources.vim-ddu-source-git_diff.src;
  };
  ddu-source-git_status = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-git_status.pname;
    version = sources.vim-ddu-source-git_status.date;
    src = sources.vim-ddu-source-git_status.src;
  };
  ddu-source-mr = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-mr.pname;
    version = sources.vim-ddu-source-mr.date;
    src = sources.vim-ddu-source-mr.src;
  };
  ddu-filter-converter_hl_dir = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-converter_hl_dir.pname;
    version = sources.vim-ddu-filter-converter_hl_dir.date;
    src = sources.vim-ddu-filter-converter_hl_dir.src;
  };
  ddu-source-git_branch = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-git_branch.pname;
    version = sources.vim-ddu-source-git_branch.date;
    src = sources.vim-ddu-source-git_branch.src;
  };
  ddu-source-git_diff_tree = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-git_diff_tree.pname;
    version = sources.vim-ddu-source-git_diff_tree.date;
    src = sources.vim-ddu-source-git_diff_tree.src;
  };
  ddu-source-git_log = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-git_log.pname;
    version = sources.vim-ddu-source-git_log.date;
    src = sources.vim-ddu-source-git_log.src;
  };
  ddu-source-file_external = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-file_external.pname;
    version = sources.vim-ddu-source-file_external.date;
    src = sources.vim-ddu-source-file_external.src;
  };
  ddu-source-help = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-help.pname;
    version = sources.vim-ddu-source-help.date;
    src = sources.vim-ddu-source-help.src;
  };
  ddu-source-highlight = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-highlight.pname;
    version = sources.vim-ddu-source-highlight.date;
    src = sources.vim-ddu-source-highlight.src;
  };
  ddu-column-icon_filename = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-column-icon_filename.pname;
    version = sources.vim-ddu-column-icon_filename.date;
    src = sources.vim-ddu-column-icon_filename.src;
  };
  ddu-source-buffer = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-buffer.pname;
    version = sources.vim-ddu-source-buffer.date;
    src = sources.vim-ddu-source-buffer.src;
  };
  ddu-source-rg = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-rg.pname;
    version = sources.vim-ddu-source-rg.date;
    src = sources.vim-ddu-source-rg.src;
  };
  ddu-filter-converter_devicon = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-filter-converter_devicon.pname;
    version = sources.vim-ddu-filter-converter_devicon.date;
    src = sources.vim-ddu-filter-converter_devicon.src;
  };
  ddu-source-lsp = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-lsp.pname;
    version = sources.vim-ddu-source-lsp.date;
    src = sources.vim-ddu-source-lsp.src;
  };
  ddu-gh_project = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-gh_project.pname;
    version = sources.vim-ddu-gh_project.date;
    src = sources.vim-ddu-gh_project.src;
  };
  ddu-source-nvim-notify = vimUtils.buildVimPlugin {
    pname = sources.vim-ddu-source-nvim-notify.pname;
    version = sources.vim-ddu-source-nvim-notify.date;
    src = sources.vim-ddu-source-nvim-notify.src;
  };
}
