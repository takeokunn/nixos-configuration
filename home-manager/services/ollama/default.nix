{
  services.ollama = {
    enable = true;
    environmentVariables = {
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_KV_CACHE_TYPE = "q8_0";
      OLLAMA_NUM_PARALLEL = "4";
      OLLAMA_MAX_LOADED_MODELS = "1";
      OLLAMA_KEEP_ALIVE = "1h";
    };
  };
}
