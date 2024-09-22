{
  programs.dust = {
    enable = true;
    config = ''
      # Print tree upside down (biggest highest)
      reverse=true

      # Subdirectories will not have their path shortened
      display-full-paths=true

      # Use file length instead of blocks
      display-apparent-size=true

      # No colors will be printed
      no-colors=true

      # No percent bars or percentages will be displayed
      no-bars=true

      # No total row will be displayed
      skip-total=true

      # Do not display hidden files
      ignore-hidden=true

      # print sizes in powers of 1000 (e.g., 1.1G)
      output-format="si"
    '';
  };
}
