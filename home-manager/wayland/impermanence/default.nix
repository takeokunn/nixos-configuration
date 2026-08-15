{
  home.persistence."/persist" = {
    directories = [
      "Downloads"
      "Documents"
      "Pictures"
      "Videos"
      "Music"
      "Desktop"

      "ghq"
      "Projects"
      "src"

      ".local/share/fish"
      ".local/share/direnv"
      ".local/share/zoxide"

      {
        directory = ".gnupg";
        mode = "0700";
      }
      {
        directory = ".ssh";
        mode = "0700";
      }
      {
        directory = ".password-store";
        mode = "0700";
      }
      {
        directory = ".local/share/keyrings";
        mode = "0700";
      }

      ".config/emacs"
      ".local/share/emacs"
      ".local/state/emacs"
      ".config/nvim"
      ".local/share/nvim"
      ".local/state/nvim"

      ".mozilla"
      ".config/chromium"
      ".config/google-chrome"

      ".config/discord"
      ".config/Slack"

      ".cargo"
      ".rustup"
      ".npm"
      ".local/share/pnpm"
      ".go"
      ".cache/go-build"
      ".config/gh"
      ".docker"

      ".local/share/containers"
      ".local/share/libvirt"

      ".local/state/nix"

      ".local/share/applications"
      ".local/share/mime"
      ".config/dconf"

      ".config/easyeffects"

      ".config/mako"
    ];
    files = [
      ".bash_history"
      ".zsh_history"
    ];
  };
}
