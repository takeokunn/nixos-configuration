# Provides `nurPkgs` to every home-manager module in the tree via the module
# system. Consumers of the exported bundles must NOT set `_module.args.nurPkgs`
# themselves; import this module (bundles do so automatically) and it will
# evaluate the nur-packages input against the active `pkgs`.
{ pkgs, nur-packages, ... }:
{
  _module.args.nurPkgs = import nur-packages { inherit pkgs; };
}
