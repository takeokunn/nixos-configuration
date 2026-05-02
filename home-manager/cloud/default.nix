let
  awscli = import ./awscli;
  k9s = import ./k9s;
in
[ awscli k9s ]
