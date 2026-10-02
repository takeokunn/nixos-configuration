{ pkgs, ... }:
{
  programs.gh.enable = true;
  programs.gh.gitCredentialHelper.enable = true;

  # Every gh on PATH runs through gh-secret-guard.sh, which refuses to send a secret. share/ keeps
  # the upstream completions and man pages; only bin/gh is replaced.
  programs.gh.package = pkgs.symlinkJoin {
    name = "gh-secret-guarded-${pkgs.gh.version}";
    paths = [ pkgs.gh ];
    postBuild = ''
      rm $out/bin/gh
      ln -s ${
        import ./gh-secret-guard.nix {
          inherit pkgs;
          gh = "${pkgs.gh}/bin/gh";
        }
      } $out/bin/gh
    '';
    inherit (pkgs.gh) meta;
  };

  programs.fish.interactiveShellInit = ''
    set -x NIX_CONFIG "access-tokens = github.com="(gh auth token)
  '';
}
