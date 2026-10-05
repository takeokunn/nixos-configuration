{
  qt.enable = true;
  # The gtk3 platform theme ships with qtbase. The gtk2 style was dropped because nixpkgs removed
  # qt6Packages.qt6gtk2 along with its gtk2 dependency.
  qt.platformTheme.name = "gtk";
}
