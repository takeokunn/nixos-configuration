{ config, lib, ... }:
let
  git = config.programs.git;

  # jj strips "JJ:" lines from the edited description, not "#" lines, so the
  # shared git commit template is re-prefixed rather than copied verbatim.
  commitTemplateLines = lib.splitString "\n" (
    lib.removeSuffix "\n" (builtins.readFile ../git/message)
  );
  toJjLine = line: if line == "" then "JJ:" else "JJ:" + lib.removePrefix "#" line;
  commitTemplate = lib.concatMapStrings (line: toJjLine line + "\n") commitTemplateLines;
in
{
  programs.jujutsu.enable = true;

  # Interactive defaults only: the ghq/worktree fish functions and Emacs
  # commands pass every fetch option explicitly and must not depend on these.
  programs.jujutsu.settings = {
    user = {
      inherit (git.settings.user) name email;
    };

    # jj never runs Git hooks, so the gitHooks checks do not cover commits made here. Signing still
    # applies, and private-commits stands in for the pre-push identity check: jj git push refuses a
    # commit the remote lacks unless its author is user.email. Secret scanning before a jj push is left
    # to the agent guardrail and the jujutsu skill.
    signing = {
      behavior = "own";
      backend = "ssh";
      key = git.signing.key;
      backends.ssh.allowed-signers = git.settings.gpg.ssh.allowedSignersFile;
    };

    # Counterpart of fetch.all: fetch every remote, not only the default one.
    git.fetch = "*";

    # A `ghq get --bare` clone has no remote.origin.fetch refspec, from which jj
    # would otherwise take the bookmarks to fetch, so it would fetch nothing.
    remotes.origin.fetch-bookmarks = "*";

    # "git" is jj's name for Git's diff3 markers (merge.conflictStyle = diff3).
    ui.conflict-marker-style = "git";

    templates.draft_commit_description = ''
      concat(
        builtin_draft_commit_description,
        ${builtins.toJSON commitTemplate},
      )
    '';

    git.private-commits = "~mine()";
  };

  programs.difftastic.jujutsu.enable = true;
}
