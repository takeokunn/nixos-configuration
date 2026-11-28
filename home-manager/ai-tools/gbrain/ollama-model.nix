# One Ollama model installed from the Nix store, given its registry manifest (a flake input).
#
# Ollama finds a model through <models>/manifests/registry.ollama.ai/library/<name>/<tag>, which
# names its blobs as <models>/blobs/sha256-<hex>. The model is tagged with its manifest digest, so
# a new manifest is a new model name and nothing that recorded the old name mistakes the two.
{ lib, pkgs }:
{ name, manifest }:
let
  manifestHex = builtins.hashFile "sha256" manifest;
  parsed = builtins.fromJSON (builtins.readFile manifest);
  tag = lib.substring 0 12 manifestHex;
  blobName = digest: "sha256-${lib.removePrefix "sha256:" digest}";
  blob =
    { digest, ... }:
    pkgs.fetchurl {
      name = "ollama-${name}-${lib.substring 7 12 digest}";
      url = "https://registry.ollama.ai/v2/library/${name}/blobs/${digest}";
      hash = digest;
    };
  # Ollama accepts a symlinked manifest only when the path it resolves to is named after the
  # manifest's digest and is also that digest's blob.
  manifestBlob = pkgs.runCommand "ollama-${name}-${tag}-manifest" { } ''
    mkdir "$out"
    cp ${manifest} "$out/sha256-${manifestHex}"
  '';
  manifestPath = "${manifestBlob}/sha256-${manifestHex}";
in
{
  # The name Ollama serves the model under.
  ollamaName = "${name}:${tag}";
  # home.file entries, relative to the home directory.
  files = {
    ".ollama/models/manifests/registry.ollama.ai/library/${name}/${tag}".source = manifestPath;
    ".ollama/models/blobs/sha256-${manifestHex}".source = manifestPath;
  }
  // lib.listToAttrs (
    map (b: lib.nameValuePair ".ollama/models/blobs/${blobName b.digest}" { source = blob b; }) (
      [ parsed.config ] ++ parsed.layers
    )
  );
}
