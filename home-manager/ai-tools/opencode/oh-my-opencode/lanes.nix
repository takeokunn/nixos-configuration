{
  mkLane =
    {
      modelTier,
      variant,
      prompt_append,
      description,
      extra ? { },
    }:
    {
      model = modelTier.model;
      fallback_models = modelTier.fallback;
      inherit variant prompt_append description;
    }
    // extra;
}
