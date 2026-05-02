{
  programs.jq = {
    enable = true;

    colors = {
      null = "0;36";
      false = "0;31";
      true = "0;35";
      numbers = "0;32";
      strings = "0;36";
      arrays = "1;35";
      objects = "1;37";
      objectKeys = "1;34";
    };
  };
}
