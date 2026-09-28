{ config, ... }:
let
  git = config.programs.git;
in
{
  programs.jujutsu.enable = true;

  programs.jujutsu.settings = {
    user = {
      inherit (git.settings.user) name email;
    };

    # jj never runs Git hooks, so the gitHooks checks do not cover commits made here; signing still does.
    signing = {
      behavior = "own";
      backend = "ssh";
      key = git.signing.key;
      backends.ssh.allowed-signers = git.settings.gpg.ssh.allowedSignersFile;
    };
  };

  programs.difftastic.jujutsu.enable = true;
}
