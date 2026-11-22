# lnav theme with the style set of lnav's built-in "dracula" theme, colored from the Stylix palette.
c:
let
  fg = color: { inherit color; };
  bold = color: {
    inherit color;
    bold = true;
  };
  on = color: background-color: { inherit color background-color; };
in
{
  styles = {
    identifier = fg "semantic()";
    text = on c.base05 c.base00;
    selected-text.background-color = c.base02;
    fuzzy-match = bold c.base09 // {
      underline = true;
    };
    alt-text.background-color = c.base01;
    ok = bold c.base0B;
    info = bold c.base0E;
    error = bold c.base08;
    warning = bold c.base0A;
    hidden = bold c.base0A;
    cursor-line = on c.base0C c.base02 // {
      bold = true;
    };
    time-column.background-color = c.base01;
    disabled-cursor-line = on c.base0C c.base01;
    adjusted-time = fg c.base0E;
    skewed-time = fg c.base0A;
    offset-time = fg c.base0C;
    time-ago.bold = true;
    file-offset = fg c.base03;
    invalid-msg = fg c.base0A;
    focused = on c.base00 c.base05;
    disabled-focused = on c.base05 c.base02;
    popup = on c.base05 c.base01;
    popup-border = on c.base05 c.base01;
    scrollbar = on c.base00 c.base05;
    timeline-bar.background-color = c.base0F;
    h1 = bold c.base0F;
    h2 = fg c.base0F // {
      underline = true;
    };
    h3 = fg c.base0F;
    h4.underline = true;
    h5.italic = true;
    h6.italic = true;
    hr = fg c.base03;
    hyperlink.underline = true;
    list-glyph = fg c.base0C;
    breadcrumb = bold c.base04;
    table-border = fg c.base02;
    table-header.bold = true;
    quote-border = on c.base03 c.base01;
    quoted-text = on c.base0A c.base01;
    footnote-border = on c.base0F c.base01;
    footnote-text = on c.base0C c.base01;
    snippet-border = fg c.base0C;
    indent-guide = fg c.base02;
  };

  syntax-styles = {
    inline-code = on c.base0B c.base01;
    quoted-code = on c.base09 c.base01;
    code-border = on c.base02 c.base01;
    object-key = fg "semantic()";
    keyword = bold c.base0E;
    string = bold c.base0A;
    comment = fg c.base03;
    doc-directive = fg c.base0E;
    variable = fg c.base09;
    symbol = fg c.base0C;
    re-special = fg c.base0C;
    re-repeat = fg c.base0A;
    diff-delete = fg c.base08;
    diff-add = fg c.base0B;
    diff-section = fg c.base03;
    spectrogram-low = bold c.base00 // {
      background-color = c.base0B;
    };
    spectrogram-medium = bold c.base00 // {
      background-color = c.base0A;
    };
    spectrogram-high = bold c.base00 // {
      background-color = c.base08;
    };
    file = fg c.base0F;
    null = fg c.base03;
    ascii-control = fg c.base0B;
    non-ascii = fg c.base0A;
    number.bold = true;
    function = fg c.base0B;
    separators-references-accessors = fg c.base0E;
    type = fg c.base0C;
  };

  status-styles = {
    disabled-title = on c.base0F c.base01 // {
      bold = true;
    };
    title = on c.base00 c.base0F // {
      bold = true;
    };
    alert-title = on c.base08 c.base0F // {
      bold = true;
    };
    subtitle = on c.base00 c.base0C // {
      bold = true;
    };
    info = on c.base04 c.base00;
    title-hotkey = on c.base00 c.base0F // {
      underline = true;
    };
    hotkey = fg c.base05 // {
      underline = true;
    };
    text = on c.base05 c.base01;
    warn = on c.base09 c.base01;
    alert = on c.base08 c.base01;
    active = on c.base0B c.base01;
    inactive = on c.base03 c.base00;
    inactive-warn = on c.base09 c.base00;
    inactive-alert = on c.base08 c.base00;
    suggestion = fg c.base03;
  };

  log-level-styles = {
    warning = fg c.base09;
    error = fg c.base08;
    critical = fg c.base08;
    fatal = fg c.base08;
  };
}
