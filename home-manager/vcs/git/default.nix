{
  xdg.configFile."git/message".source = ./message;

  programs.gitHooks.enable = true;

  programs.gitleaks.enable = true;
  programs.gitleaks.settings.extend.useDefault = true;

  programs.git.enable = true;
  programs.git.lfs.enable = true;
  programs.git.maintenance.enable = true;

  programs.git.ignores = [
    "*.swp"
    "*.save"
    "*.fasl"
    "*.elc"
    "*~"
    ".DS_Store"
    ".dir-locals-2.el"
    "TAGS"
    ".neotree"
    ".direnv"
    ".devenv"
    "devenv.yaml"
    "devenv.nix"
    "devenv.lock"
    ".devenv.flake.nix"
    ".pre-commit-config.yaml"
    ".git-hooks-config.yaml"
    ".markdown-preview.html"
    ".serena/"
    ".sisyphus/"
    ".cache/"
    "**/.claude/settings.local.json"
    "coverage.out"
    ".auth/"
    "playwright-report/"
    "test-results/"
  ];

  programs.git.signing.key = "~/.ssh/signing_key.pub";
  programs.git.signing.format = "ssh";
  programs.git.signing.signByDefault = true;

  programs.git.includes = [
    { path = "~/.config/git/config.d/maintenance.conf"; }
    # Work account (takeokunn-attmcojp) scoped to ~/ghq/github.com/attmcojp/.
    # Identity uses the GitHub noreply address so it is safe to commit in this
    # public repo. Auth is delegated to gh over HTTPS for the work account, and
    # the pushInsteadOf identity mapping cancels the global https->ssh rewrite
    # for attmcojp URLs only (git resolves by longest-prefix match).
    {
      condition = "gitdir:~/ghq/github.com/attmcojp/";
      contents = {
        user.name = "takeokunn-attmcojp";
        user.email = "307919810+takeokunn-attmcojp@users.noreply.github.com";
        user.signingkey = "~/.ssh/work_signing_key.pub";
        credential."https://github.com".username = "takeokunn-attmcojp";
        url."https://github.com/attmcojp/".pushInsteadOf = "https://github.com/attmcojp/";
      };
    }
  ];

  programs.git.settings = {
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

    user.name = "takeokunn";
    user.email = "bararararatty@gmail.com";

    core.quotepath = "off";
    core.ignorecase = false;
    core.safecrlf = true;
    core.precomposeunicode = true;
    core.untrackedCache = true;
    core.preloadindex = true;

    commit.template = "~/.config/git/message";

    ghq.root = "~/ghq";

    fetch.prune = true;
    fetch.pruneTags = true;
    fetch.all = true;
    fetch.writeCommitGraph = true;

    pull.rebase = false;

    push.default = "nothing";

    github.user = "takeokunn";

    gpg.ssh.allowedSignersFile = "~/.config/git/allowed_signers";

    diff.patience = true;

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
      blame.markIgnoredLines = true;
    };

    url."git@github.com:".pushInsteadOf = "https://github.com/";

    # Work (attmcojp) repos must transport over HTTPS so the gh work-account
    # credential helper authenticates them (see the attmcojp includeIf above).
    # This rewrites any SSH-form attmcojp URL to HTTPS at clone/fetch/push time,
    # so `ghq get -p attmcojp/...` still lands on HTTPS. Scoped to attmcojp only,
    # so personal repos keep using SSH exactly as before.
    url."https://github.com/attmcojp/".insteadOf = [
      "git@github.com:attmcojp/"
      "ssh://git@github.com/attmcojp/"
    ];

    init.defaultBranch = "main";

    rerere.enabled = true;

    rebase.autoStash = true;
    rebase.abbreviateCommands = false;

    merge.conflictStyle = "diff3";
  };

  programs.difftastic.enable = true;
  programs.difftastic.git.enable = true;
  programs.difftastic.git.mode = "both";
}
