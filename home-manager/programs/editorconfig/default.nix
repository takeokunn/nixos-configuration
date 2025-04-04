{ pkgs }:
{
  home.packages = with pkgs; [ editorconfig-core-c ];
  editorconfig = {
    enable = true;
    settings = {
      "*" = {
        charset = "utf-8";
        end_of_line = "lf";
        insert_final_newline = true;
        trim_trailing_whitespace = true;
        indent_style = "space";
      };
      "{Makefile, *.mk}" = {
        indent_style = "tab";
        indent_size = 4;
      };
      "nginx.conf" = {
        indent_size = 4;
        indent_style = "tab";
      };
      "mpd.conf" = {
        indent_size = 4;
        indent_style = "tab";
      };
      "*.{yml,yaml}" = {
        indent_size = 2;
      };
      "*.tf" = {
        indent_size = 2;
      };
      "*.scala" = {
        indent_size = 2;
      };
      "*.json" = {
        indent_size = 2;
      };
      "*.{el,lisp,asd}" = {
        max_line_length = 80;
      };
      "*.v" = {
        indent_size = 4;
        indent_style = "tab";
      };
      "*.{ts,tsx}" = {
        indent_size = 2;
      };
      "*.toml" = {
        indent_size = 2;
      };
      "*.xml" = {
        indent_size = 2;
      };
      "*.jsonnet" = {
        indent_size = 2;
      };
      "*.nix" = {
        indent_size = 2;
      };
    };
  };
}
