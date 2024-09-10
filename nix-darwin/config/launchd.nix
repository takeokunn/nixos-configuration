{ pkgs }: {
  launchd.agents.ollama = {
    serviceConfig = {
      ProgramArguments = [ "${pkgs.ollama}/bin/ollama" "serve" ];
      KeepAlive = true;
      RunAtLoad = true;
      EnvironmentVariables.OLLAMA_HOST = "0.0.0.0";
    };
  };
}
