{ nurPkgs, username, ... }:
{
  system.activationScripts.extraActivation.text = ''
    sudo -u ${username} osascript -e 'tell application "System Events" to tell every desktop to set picture to "${nurPkgs.dracula-wallpaper}/first-collection/macos.png"'
  '';
}
