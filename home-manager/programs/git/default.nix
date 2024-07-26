{
  home.file.".config/git/message".source = ./message;

  programs.git = {
    enable = true;
    lfs.enable = true;

    aliases = {
      st = "status";
      br = "branch";
      co = "commit";
      ch = "checkout";
      ad = "add";
      rs = "restore";
      sw = "switch";
      fix = "commit --amend --no-edit";
    };

    ignores = [ "*.swp" "*.save" "*.fasl" "*~" ".DS_Store" ];

    extraConfig = {
      core = {
        editor = "emacs -nw";
        quotepath = "off";
        ignorecase = false;
        safecrlf = true;
        autocrlf = false;
        precomposeunicode = true;
        untrackedCache = true;
        fsmonitor = true;
      };

      commit = {
        template = "~/.config/git/message";
        gpgsign = true;
      };

      user = {
        name = "takeokunn";
        email = "bararararatty@gmail.com";
        signingkey = "0B10DAA7BA0236D7382287660F79C0AB03FD7A1C";
      };

      ghq = { root = "~/.ghq"; };

      fetch = {
        prune = true;
        writeCommitGraph = true;
      };

      pull = { rebase = false; };

      push = { default = "nothing"; };

      github = { user = "takeokunn"; };

      gpg = { program = "gpg"; };

      diff = { patience = true; };

      color = {
        ui = "auto";
        status = "auto";
        diff = "auto";
        branch = "auto";
        interactive = "auto";
        grep = "auto";
      };

      url = { "git@github.com:".insteadOf = "https://github.com/"; };

      init = { defaultBranch = "main"; };

      rerere = { enabled = true; };

      rebase = {
        autoStash = true;
        abbreviateCommands = true;
        autosquash = true;
      };
    };
  };
}
