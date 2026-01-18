{ pkgs }:
{
  programs.yazi = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;

    settings = {
      manager = {
        ratio = [ 1 4 3 ];
        sort_by = "natural";
        sort_sensitive = false;
        sort_reverse = false;
        sort_dir_first = true;
        linemode = "size";
        show_hidden = false;
        show_symlink = true;
      };

      preview = {
        tab_size = 2;
        max_width = 600;
        max_height = 900;
        image_filter = "triangle";
        image_quality = 75;
        sixel_fraction = 15;
      };

      opener = {
        edit = [
          { run = "\${EDITOR:-vim} \"$@\""; block = true; for = "unix"; }
        ];
        open = [
          { run = "xdg-open \"$@\""; for = "linux"; }
        ];
        reveal = [
          { run = "nautilus --select \"$1\""; for = "linux"; }
        ];
      };

      select = {
        open_title = "Open with:";
        open_origin = "hovered";
        open_offset = [ 0 1 50 7 ];
      };

      input = {
        cursor_blink = false;
      };
    };

    # Vim-style keybindings (mostly default, with custom additions)
    keymap = {
      manager.prepend_keymap = [
        # Quick navigation
        { on = [ "g" "h" ]; run = "cd ~"; desc = "Go to home"; }
        { on = [ "g" "c" ]; run = "cd ~/.config"; desc = "Go to config"; }
        { on = [ "g" "d" ]; run = "cd ~/Downloads"; desc = "Go to downloads"; }
        { on = [ "g" "D" ]; run = "cd ~/Documents"; desc = "Go to documents"; }
        { on = [ "g" "p" ]; run = "cd ~/Pictures"; desc = "Go to pictures"; }
        { on = [ "g" "s" ]; run = "cd ~/src"; desc = "Go to src"; }
        { on = [ "g" "t" ]; run = "cd /tmp"; desc = "Go to tmp"; }
        { on = [ "g" "n" ]; run = "cd ~/ghq/github.com/takeokunn/nixos-configuration"; desc = "Go to nixos-config"; }

        # Toggle hidden files
        { on = [ "." ]; run = "hidden toggle"; desc = "Toggle hidden files"; }

        # Create
        { on = [ "a" ]; run = "create"; desc = "Create file/dir"; }
        { on = [ "A" ]; run = "create --dir"; desc = "Create directory"; }

        # Rename
        { on = [ "c" "w" ]; run = "rename --cursor=before_ext"; desc = "Rename (before ext)"; }
        { on = [ "c" "c" ]; run = "rename --cursor=start"; desc = "Rename (start)"; }
        { on = [ "c" "$" ]; run = "rename --cursor=end"; desc = "Rename (end)"; }

        # Selection
        { on = [ "V" ]; run = "select_all --state=true"; desc = "Select all"; }
        { on = [ "U" ]; run = "select_all --state=false"; desc = "Unselect all"; }

        # Shell commands
        { on = [ "!" ]; run = "shell --interactive"; desc = "Shell command (interactive)"; }
        { on = [ ":" ]; run = "shell --block"; desc = "Shell command (block)"; }

        # Sorting
        { on = [ "s" "n" ]; run = "sort natural --dir-first"; desc = "Sort by name"; }
        { on = [ "s" "N" ]; run = "sort natural --reverse --dir-first"; desc = "Sort by name (reverse)"; }
        { on = [ "s" "s" ]; run = "sort size --dir-first"; desc = "Sort by size"; }
        { on = [ "s" "S" ]; run = "sort size --reverse --dir-first"; desc = "Sort by size (reverse)"; }
        { on = [ "s" "m" ]; run = "sort modified --dir-first"; desc = "Sort by modified"; }
        { on = [ "s" "M" ]; run = "sort modified --reverse --dir-first"; desc = "Sort by modified (reverse)"; }

        # Filter
        { on = [ "f" ]; run = "filter --smart"; desc = "Filter"; }

        # Linemode toggle
        { on = [ "m" "s" ]; run = "linemode size"; desc = "Show size"; }
        { on = [ "m" "p" ]; run = "linemode permissions"; desc = "Show permissions"; }
        { on = [ "m" "m" ]; run = "linemode mtime"; desc = "Show mtime"; }
        { on = [ "m" "n" ]; run = "linemode none"; desc = "Hide linemode"; }
      ];

      tasks.prepend_keymap = [
        { on = [ "j" ]; run = "arrow 1"; desc = "Move down"; }
        { on = [ "k" ]; run = "arrow -1"; desc = "Move up"; }
      ];

      select.prepend_keymap = [
        { on = [ "j" ]; run = "arrow 1"; desc = "Move down"; }
        { on = [ "k" ]; run = "arrow -1"; desc = "Move up"; }
      ];

      input.prepend_keymap = [
        { on = [ "<Esc>" ]; run = "close"; desc = "Close input"; }
        { on = [ "<C-c>" ]; run = "close"; desc = "Close input"; }
      ];

      completion.prepend_keymap = [
        { on = [ "<C-j>" ]; run = "arrow 1"; desc = "Move down"; }
        { on = [ "<C-k>" ]; run = "arrow -1"; desc = "Move up"; }
      ];

      help.prepend_keymap = [
        { on = [ "j" ]; run = "arrow 1"; desc = "Move down"; }
        { on = [ "k" ]; run = "arrow -1"; desc = "Move up"; }
      ];
    };

    # Dracula theme
    theme = {
      manager = {
        cwd = { fg = "#50fa7b"; };
        hovered = { bg = "#44475a"; };
        preview_hovered = { bg = "#44475a"; };

        find_keyword = { fg = "#f1fa8c"; bold = true; };
        find_position = { fg = "#ff79c6"; };

        marker_copied = { fg = "#50fa7b"; bg = "#50fa7b"; };
        marker_cut = { fg = "#ff5555"; bg = "#ff5555"; };
        marker_marked = { fg = "#8be9fd"; bg = "#8be9fd"; };
        marker_selected = { fg = "#f1fa8c"; bg = "#f1fa8c"; };

        tab_active = { fg = "#282a36"; bg = "#bd93f9"; };
        tab_inactive = { fg = "#f8f8f2"; bg = "#44475a"; };
        tab_width = 1;

        count_copied = { fg = "#282a36"; bg = "#50fa7b"; };
        count_cut = { fg = "#282a36"; bg = "#ff5555"; };
        count_selected = { fg = "#282a36"; bg = "#f1fa8c"; };

        border_symbol = "│";
        border_style = { fg = "#6272a4"; };
      };

      status = {
        separator_open = "";
        separator_close = "";
        separator_style = { fg = "#44475a"; bg = "#44475a"; };

        mode_normal = { fg = "#282a36"; bg = "#bd93f9"; bold = true; };
        mode_select = { fg = "#282a36"; bg = "#50fa7b"; bold = true; };
        mode_unset = { fg = "#282a36"; bg = "#ff5555"; bold = true; };

        progress_label = { fg = "#f8f8f2"; bold = true; };
        progress_normal = { fg = "#bd93f9"; bg = "#44475a"; };
        progress_error = { fg = "#ff5555"; bg = "#44475a"; };

        permissions_t = { fg = "#bd93f9"; };
        permissions_r = { fg = "#f1fa8c"; };
        permissions_w = { fg = "#ff5555"; };
        permissions_x = { fg = "#50fa7b"; };
        permissions_s = { fg = "#6272a4"; };
      };

      input = {
        border = { fg = "#bd93f9"; };
        title = { fg = "#bd93f9"; };
        value = { fg = "#f8f8f2"; };
        selected = { reversed = true; };
      };

      select = {
        border = { fg = "#bd93f9"; };
        active = { fg = "#ff79c6"; bold = true; };
        inactive = { fg = "#f8f8f2"; };
      };

      tasks = {
        border = { fg = "#bd93f9"; };
        title = { fg = "#bd93f9"; };
        hovered = { fg = "#ff79c6"; underline = true; };
      };

      which = {
        cols = 3;
        mask = { bg = "#282a36"; };
        cand = { fg = "#8be9fd"; };
        rest = { fg = "#6272a4"; };
        desc = { fg = "#ff79c6"; };
        separator = " → ";
        separator_style = { fg = "#6272a4"; };
      };

      help = {
        on = { fg = "#8be9fd"; };
        run = { fg = "#ff79c6"; };
        desc = { fg = "#6272a4"; };
        hovered = { bg = "#44475a"; bold = true; };
        footer = { fg = "#6272a4"; bg = "#282a36"; };
      };

      notify = {
        title_info = { fg = "#50fa7b"; };
        title_warn = { fg = "#f1fa8c"; };
        title_error = { fg = "#ff5555"; };
      };

      filetype = {
        rules = [
          { mime = "image/*"; fg = "#8be9fd"; }
          { mime = "video/*"; fg = "#f1fa8c"; }
          { mime = "audio/*"; fg = "#f1fa8c"; }
          { mime = "application/zip"; fg = "#ff79c6"; }
          { mime = "application/gzip"; fg = "#ff79c6"; }
          { mime = "application/x-tar"; fg = "#ff79c6"; }
          { mime = "application/x-bzip"; fg = "#ff79c6"; }
          { mime = "application/x-bzip2"; fg = "#ff79c6"; }
          { mime = "application/x-7z-compressed"; fg = "#ff79c6"; }
          { mime = "application/x-rar"; fg = "#ff79c6"; }
          { name = "*.rs"; fg = "#ffb86c"; }
          { name = "*.go"; fg = "#8be9fd"; }
          { name = "*.nix"; fg = "#bd93f9"; }
          { name = "*.py"; fg = "#50fa7b"; }
          { name = "*.js"; fg = "#f1fa8c"; }
          { name = "*.ts"; fg = "#8be9fd"; }
          { name = "*.md"; fg = "#6272a4"; }
          { name = "*"; fg = "#f8f8f2"; }
        ];
      };
    };
  };

  home.packages = with pkgs; pkgs.lib.optionals pkgs.stdenv.isLinux [
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
