{ pkgs }:
{
  home = {
    file.".config/git/message".source = ./message;
    packages = with pkgs; [ git-secrets ];
  };

  programs.git = {
    enable = true;
    lfs.enable = true;
    maintenance.enable = true;

    delta = {
      enable = true;
      options = {
        navigate = true;
        dark = true;
        line-numbers = true;
        side-by-side = true;
      };
    };

    aliases = {
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

    ignores = [
      "*.swp"
      "*.save"
      "*.fasl"
      "*~"
      ".DS_Store"
      ".claude/"
      ".dir-locals-2.el"
      ".aider*"
      "TAGS"
      ".neotree"
      ".direnv"
      ".devenv"
      ".pre-commit-config.yaml"
    ];

    userName = "takeokunn";
    userEmail = "bararararatty@gmail.com";

    signing = {
      key = "0B10DAA7BA0236D7382287660F79C0AB03FD7A1C";
      format = "openpgp";
      signByDefault = true;
    };

    includes = [
      { path = "~/.config/git/config.d/maintenance.conf"; }
    ];

    extraConfig = {
      core = {
        quotepath = "off";
        ignorecase = false;
        safecrlf = true;
        autocrlf = false;
        precomposeunicode = true;
        untrackedCache = true;
        fsmonitor = true;
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

      secrets = {
        providers = "git secrets --aws-provider";
        patterns = [
          "(A3T[A-Z0-9]|AKIA|AGPA|AIDA|AROA|AIPA|ANPA|ANVA|ASIA)[A-Z0-9]{16}"
          "(\"|')?(AWS|aws|Aws)?_?(SECRET|secret|Secret)?_?(ACCESS|access|Access)?_?(KEY|key|Key)(\"|')?\\s*(:|=>|=)\\s*(\"|')?[A-Za-z0-9/\\+=]{40}(\"|')?"
          "(\"|')?(AWS|aws|Aws)?_?(ACCOUNT|account|Account)_?(ID|id|Id)?(\"|')?\\s*(:|=>|=)\\s*(\"|')?[0-9]{4}\\-?[0-9]{4}\\-?[0-9]{4}(\"|')?"
        ];
      };
    };
  };
}
