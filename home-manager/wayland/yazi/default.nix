{ pkgs, ... }:
let
  bind = on: run: desc: { inherit on run desc; };
in
{
  programs.yazi.enable = true;
  programs.yazi.shellWrapperName = "y";
  stylix.targets.yazi.enable = true;

  # Only values that differ from yazi's presets; section names follow yazi 25.5+ (mgr, pick, cmp).
  programs.yazi.settings.mgr = {
    sort_by = "natural";
    linemode = "size";
  };

  programs.yazi.keymap = {
    mgr.prepend_keymap = [
      (bind [ "g" "D" ] "cd ~/Documents" "Go to documents")
      (bind [ "g" "p" ] "cd ~/Pictures" "Go to pictures")
      (bind [ "g" "s" ] "cd ~/src" "Go to src")
      (bind [ "g" "t" ] "cd /tmp" "Go to tmp")
      (bind [
        "g"
        "n"
      ] "cd ~/ghq/github.com/takeokunn/nixos-configuration.git/.worktrees" "Go to nixos-config worktrees")

      (bind [ "A" ] "create --dir" "Create directory")
      (bind [ "c" "w" ] "rename --cursor=before_ext" "Rename (before extension)")
      (bind [ "c" "c" ] "rename --cursor=start" "Rename (from start)")
      (bind [ "c" "$" ] "rename --cursor=end" "Rename (from end)")

      (bind [ "V" ] "toggle_all --state=on" "Select all")
      (bind [ "U" ] "toggle_all --state=off" "Unselect all")

      (bind [ "!" ] "shell --interactive" "Run shell command")
    ];

    input.prepend_keymap = [
      (bind [ "<Esc>" ] "close" "Cancel input")
    ];

    cmp.prepend_keymap = [
      (bind [ "<C-j>" ] "arrow next" "Next completion")
      (bind [ "<C-k>" ] "arrow prev" "Previous completion")
    ];
  };

  home.packages = with pkgs; [
    # Preview dependencies
    file
    unar
    poppler
    ffmpegthumbnailer
    fd
    ripgrep
    fzf
    jq
    imagemagick
  ];
}
