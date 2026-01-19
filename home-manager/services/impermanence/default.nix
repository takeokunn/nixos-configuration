{ pkgs, ... }:
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

      # Editors
      ".config/emacs"
      ".local/share/emacs"
      ".local/state/emacs"
      ".config/nvim"
      ".local/share/nvim"
      ".local/state/nvim"

      # Browsers
      ".mozilla"
      ".config/chromium"
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
      ".docker"

      # Containers & VMs
      ".local/share/containers"
      ".local/share/libvirt"

      # Nix
      ".local/state/nix"

      # Application data
      ".local/share/applications"
      ".local/share/mime"
      ".config/dconf"

      # Wayland/niri specific
      # Note: .config/niri is managed by Home Manager, not persisted
      # to prevent stale configs (e.g., duplicate waybar spawns)
      ".config/mako"
    ];
    files = [
      ".bash_history"
      ".zsh_history"
    ];
  };
}
