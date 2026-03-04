{
  pkgs,
  emacsPkg,
  emacsLib,
  nur-packages,
}:
let
  lib = pkgs.lib;
  nurPkgs = nur-packages.packages.${pkgs.system};
  emacs = import ./emacs { inherit pkgs emacsPkg; };
  gpg-agent = import ./gpg-agent { inherit pkgs; };

  # Modern services (niri ecosystem)
  hypridle = import ./hypridle { inherit pkgs; };
  hyprlock = import ./hyprlock { inherit pkgs; };
  swayosd = import ./swayosd { inherit pkgs; };
  wlsunset = import ./wlsunset { inherit pkgs; };
  kanshi = import ./kanshi { inherit pkgs; };
  playerctld = import ./playerctld { inherit pkgs; };
  cliphist = import ./cliphist { inherit pkgs; };
  wl-clip-persist = import ./wl-clip-persist { inherit pkgs; };
  easyeffects = import ./easyeffects { inherit pkgs lib; };
  mako = import ./mako { inherit pkgs; };

  # Impermanence (Linux only)
  impermanence = import ./impermanence { inherit pkgs; };

  # Darwin only
  lmstudio = import ./lmstudio;
  lmstudioPackage = { ... }: { services.lmstudio.package = nurPkgs.lmstudio; };
in
[
  emacs
  gpg-agent

  # Modern services (niri ecosystem)
  hypridle
  hyprlock
  swayosd
  wlsunset
  kanshi
  playerctld
  cliphist
  wl-clip-persist
  easyeffects
  mako
]
++ pkgs.lib.optionals pkgs.stdenv.isLinux [
  # Impermanence (Linux only)
  impermanence
]
++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
  # Darwin only
  lmstudio
  lmstudioPackage
]
