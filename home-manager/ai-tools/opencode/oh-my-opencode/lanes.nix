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
      inherit variant prompt_append description;
    }
    // extra;
}
