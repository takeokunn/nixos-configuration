# The home.file entries that install one Ollama model under ~/.ollama/models from the Nix store.
#
# Ollama finds a model through <models>/manifests/registry.ollama.ai/library/<name>/<tag>, which
# names its blobs as <models>/blobs/sha256-<hex>. Linking each file rather than pointing
# OLLAMA_MODELS at the store keeps ~/.ollama/models writable for models pulled by hand.
{ lib, pkgs }:
model:
let
  blobName = digest: "sha256-${lib.removePrefix "sha256:" digest}";
  blob =
    { digest, ... }:
    pkgs.fetchurl {
      name = "ollama-${model.name}-${lib.substring 7 12 digest}";
      url = "https://registry.ollama.ai/v2/library/${model.name}/blobs/${digest}";
      hash = digest;
    };
  # The registry serves a manifest by tag only, so its bytes are rebuilt here in the registry's
  # key order and checked against the registry's digest as a fixed output.
  layerJson = b: ''{"mediaType":"${b.mediaType}","digest":"${b.digest}","size":${toString b.size}}'';
  manifest = pkgs.runCommand "ollama-${model.name}-${model.tag}-manifest" {
    json = ''{"schemaVersion":2,"mediaType":"application/vnd.docker.distribution.manifest.v2+json","config":${layerJson model.config},"layers":[${
      lib.concatMapStringsSep "," layerJson model.layers
    }]}'';
    outputHashAlgo = "sha256";
    outputHash = lib.removePrefix "sha256:" model.manifestDigest;
  } ''printf '%s' "$json" >"$out"'';
  # Ollama accepts a symlinked manifest only when the path it resolves to is named after the
  # manifest's digest and is also that digest's blob.
  manifestBlob = pkgs.runCommand "ollama-${model.name}-${model.tag}-manifest-blob" { } ''
    mkdir "$out"
    cp ${manifest} "$out/${blobName model.manifestDigest}"
  '';
  manifestPath = "${manifestBlob}/${blobName model.manifestDigest}";
in
{
  ".ollama/models/manifests/registry.ollama.ai/library/${model.name}/${model.tag}".source =
    manifestPath;
  ".ollama/models/blobs/${blobName model.manifestDigest}".source = manifestPath;
}
// lib.listToAttrs (
  map (b: lib.nameValuePair ".ollama/models/blobs/${blobName b.digest}" { source = blob b; }) (
    [ model.config ] ++ model.layers
  )
)
