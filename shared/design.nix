# Design values shared by the X13Gen2 desktop: bar, launchers, notifications, OSD, lock and login
# screens. Font names and the base sizes come from Stylix, so changing Stylix moves every surface;
# the larger sizes and the radii are fixed steps that every surface picks from.
stylixFonts: {
  font = {
    sans = stylixFonts.sansSerif.name;
    mono = stylixFonts.monospace.name;
    # Tabular figures for clocks and workspace numbers: the digits keep one width, so a clock does
    # not shift as it ticks, and Geist keeps its plain zero (Geist Mono's is slashed).
    figures = "tnum";
    icons = "Symbols Nerd Font";
  };

  # Point sizes, smallest first.
  size = {
    caption = stylixFonts.sizes.desktop - 1; # values under rail icons, the rail clock's date
    body = stylixFonts.sizes.desktop; # bar text, OSD, tooltips
    popup = stylixFonts.sizes.popups; # launchers, notifications
    label = stylixFonts.sizes.terminal; # workspace numbers
    subtitle = 14; # tray handle, lock screen hint and track
    title = 16; # rail icons, secondary lines on the lock and login screens
    clock = 24; # rail clock
    heading = 30; # date on the lock screen
    loginClock = 120;
    lockClock = 200;
  };

  # Dates on the lock and login screens.
  date = {
    format = "%A, %B %-d";
    locale = "en_US";
  };

  # Corner radii in pixels.
  radius = {
    track = 4; # OSD progress bar, half its 8px height
    button = 11; # rail workspace buttons
    chip = 12; # rail status chips, tooltips
    notification = 14;
    window = 16; # windows, launchers, the login panel
    rail = 18;
    osd = 25; # OSD band, half its 50px height
  };
}
