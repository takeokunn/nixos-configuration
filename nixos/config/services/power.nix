{
  services.auto-epp = {
    enable = true;
    settings.Settings = {
      epp_state_for_AC = "balance_performance";
      epp_state_for_BAT = "power";
    };
  };
}
