{
  home.persistence."/persist" = {
    directories = [
      # XDG directories
      "Downloads"
      "Documents"
      "Pictures"
      "Videos"
      "Music"
      "Desktop"

      # Development
      "ghq"
      "Projects"
      "src"

      # Shell & terminal
      ".local/share/fish"
      ".local/share/direnv"
      ".local/share/zoxide"

      # Security (with restricted permissions)
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
      # The TPM-sealed credential oo7 unlocks the keyring with.
      {
        directory = ".config/credstore.encrypted";
        mode = "0700";
      }

      # Editors
      ".config/emacs"
      ".local/share/emacs"
      ".local/state/emacs"
      ".config/nvim"
      ".local/share/nvim"
      ".local/state/nvim"

      # Browsers
      ".mozilla"
      ".config/google-chrome"

      # Communication
      ".config/discord"
      ".config/Slack"

      # Development tools
      ".cargo"
      ".rustup"
      ".npm"
      ".local/share/pnpm"
      ".go"
      ".cache/go-build"
      ".config/gh"

      # Secrets editing key (sops)
      {
        directory = ".config/sops/age";
        mode = "0700";
      }

      # Launcher, audio state, current wallpaper
      ".local/share/vicinae"
      ".local/state/wireplumber"
      ".local/state/awww"

      # AI tools
      ".claude"
      ".config/codex"
      {
        directory = ".gbrain";
        mode = "0700";
      }
      ".local/share/opencode"

      # Cloud credentials and mail
      ".aws"
      ".kube"
      "Maildir"

      # Containers & VMs
      ".local/share/containers"

      # Nix
      ".local/state/nix"

      # Application data
      ".local/share/applications"
      ".local/share/mime"
      ".config/dconf"
      # Nautilus scripts, starred files and script shortcuts; its view settings live in dconf.
      ".local/share/nautilus"
      ".config/nautilus"
      # Nautilus sidebar bookmarks (gtk-3.0/bookmarks); GTK rewrites the file by rename, so the
      # directory is persisted rather than the file.
      ".config/gtk-3.0"

      # Audio: per-device preset rules saved from the EasyEffects window
      ".local/share/easyeffects/autoload"

    ];
    files = [
      ".claude.json"
      ".bash_history"
    ];
  };
}
