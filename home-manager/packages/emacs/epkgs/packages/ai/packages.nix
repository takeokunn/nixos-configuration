{ sources, epkgs }:
{
  copilot = epkgs.melpaBuild {
    pname = "copilot";
    version = "0.0.1";
    src = sources.emacs-copilot.src;

    packageRequires = with epkgs; [
      editorconfig
      f
      jsonrpc
    ];

    ignoreCompilationError = false;
  };
}
