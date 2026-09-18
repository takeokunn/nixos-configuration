{
  # macOS launchd agent sets TMPDIR=/tmp, so socket is created here
  socketPath = "/tmp/emacs$(id -u)/server";

  defaultWindowWidth = 900;
  defaultWindowHeight = 600;

  defaultAppId = "FloatingEmacs";

  scratchpadInstanceGroup = "emacs-scratchpad";
}
