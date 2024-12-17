{ pkgs }:
{
  home = {
    file.".config/git/message".source = ./message;
    packages = with pkgs; [ git-secrets ];
  };

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

    ignores = [
      "*.swp"
      "*.save"
      "*.fasl"
      "*~"
      ".DS_Store"
    ];

    extraConfig = {
      core = {
        quotepath = "off";
        ignorecase = false;
        safecrlf = true;
        autocrlf = false;
        precomposeunicode = true;
        untrackedCache = true;
        fsmonitor = false;
      };

      commit = {
        template = "~/.config/git/message";
        gpgsign = true;
      };

      tag = {
        gpgsign = true;
      };

      user = {
        name = "takeokunn";
        email = "bararararatty@gmail.com";
        signingkey = "0B10DAA7BA0236D7382287660F79C0AB03FD7A1C";
      };

      ghq = {
        root = "~/.ghq";
      };

      fetch = {
        prune = true;
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
        autosquash = true;
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
