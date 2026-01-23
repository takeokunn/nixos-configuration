{
  programs.cargo = {
    enable = true;
    settings = {
      build.jobs = 16;
      profile.dev.codegen-units = 16;
    };
  };
}
