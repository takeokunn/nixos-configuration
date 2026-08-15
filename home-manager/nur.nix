{ pkgs, nur-packages, ... }:
{
  _module.args.nurPkgs = import nur-packages { inherit pkgs; };
}
