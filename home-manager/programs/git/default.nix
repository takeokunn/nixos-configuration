{
  home.file.".config/git/message".source = ./message;

  programs.gitleaks = {
    enable = true;
    enableGitHook = true;
    settings = {
      extend.useDefault = true;
    };
  };

  programs.git = {
    enable = true;
    lfs.enable = true;
    maintenance.enable = true;

    ignores = [
      "*.swp"
      "*.save"
      "*.fasl"
      "*~"
      ".DS_Store"
      ".dir-locals-2.el"
      "TAGS"
      ".neotree"
      ".direnv"
      ".devenv"
      ".pre-commit-config.yaml"
      ".markdown-preview.html"
      ".serena/"
      ".cache/"
      "**/.claude/settings.local.json"
      "coverage.out"
    ];

    signing = {
      key = "0B10DAA7BA0236D7382287660F79C0AB03FD7A1C";
      format = "openpgp";
      signByDefault = true;
    };

    includes = [
      { path = "~/.config/git/config.d/maintenance.conf"; }
    ];

    settings = {
      # エイリアス設定（旧 aliases）
      alias = {
        st = "status --ignore-submodules=all";
        br = "branch";
        co = "commit";
        ch = "checkout";
        ad = "add";
        rs = "restore";
        sw = "switch";
        fix = "commit --amend --no-edit";
        fixup = "!git log --oneline -n 20 | peco | awk '{print $1}' | xargs --no-run-if-empty -I COMMIT -o sh -c 'git commit --fixup COMMIT && git rebase -i --autosquash COMMIT~'";
      };

      # ユーザー情報（旧 userName, userEmail）
      user = {
        name = "takeokunn";
        email = "bararararatty@gmail.com";
      };

      # 以下は旧 extraConfig の内容
      core = {
        quotepath = "off";
        ignorecase = false;
        safecrlf = true;
        autocrlf = false;
        precomposeunicode = true;
        untrackedCache = true;
        fsmonitor = false;
        preloadindex = true;
      };

      commit = {
        template = "~/.config/git/message";
      };

      ghq = {
        root = "~/ghq";
      };

      fetch = {
        prune = true;
        pruneTags = true;
        all = true;
        writeCommitGraph = true;
      };

      pull = {
        rebase = false;
      };

      push = {
        default = "nothing";
      };

      github = {
        user = "takeokunn";
      };

      gpg = {
        program = "gpg";
      };

      diff = {
        patience = true;
      };

      # thanks https://github.com/dracula/git/blob/master/config/gitconfig
      color = {
        ui = "auto";
        status = {
          added = "green";
          changed = "yellow";
          header = "";
          localBranch = "";
          nobranch = "";
          remoteBranch = "cyan bold";
          unmerged = "magenta bold reverse";
          untracked = "red";
          updated = "green bold";
        };

        diff = {
          commit = "";
          func = "cyan";
          plain = "";
          whitespace = "magenta reverse";
          meta = "white";
          frag = "cyan bold reverse";
          old = "red";
          new = "green";
        };

        branch = {
          current = "cyan bold reverse";
          local = "white";
          plain = "";
          remote = "cyan";
        };

        interactive = {
          error = "";
          header = "";
          help = "";
          prompt = "";
        };

        grep = {
          context = "";
          filename = "";
          function = "";
          linenumber = "white";
          match = "";
          selected = "";
          separator = "";
        };

        blame = {
          markIgnoredLines = true;
        };
      };

      url = {
        "git@github.com:".insteadOf = "https://github.com/";
      };

      init = {
        defaultBranch = "main";
      };

      rerere = {
        enabled = true;
      };

      rebase = {
        autoStash = true;
        abbreviateCommands = false;
      };

      merge = {
        conflictStyle = "diff3";
      };

    };
  };

  programs.difftastic = {
    enable = true;
    git = {
      enable = true;
      diffToolMode = true;
    };
  };
}
