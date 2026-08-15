{
  # macOS launchd agent sets TMPDIR=/tmp, so socket is created here
  # Format uses shell variable expansion: $(id -u) gets current user ID
  socketPath = "/tmp/emacs$(id -u)/server";

  defaultWindowWidth = 900;
  defaultWindowHeight = 600;

  defaultAppId = "FloatingEmacs";
}
