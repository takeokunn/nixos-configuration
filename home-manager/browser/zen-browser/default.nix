{
  pkgs,
  firefox-addons,
}:
let
  addons = firefox-addons.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  programs.zen-browser.enable = true;
  programs.zen-browser.darwinDefaultsId = "app.zen-browser.zen";
  programs.zen-browser.policies.DisableAppUpdate = true;
  programs.zen-browser.policies.DisableTelemetry = true;
  programs.zen-browser.policies.EnableTrackingProtection.Value = true;
  programs.zen-browser.policies.EnableTrackingProtection.Locked = true;

  programs.zen-browser.profiles.default = {
    isDefault = true;

    extensions.packages = with addons; [
      ublock-origin
      vimium
    ];

    userChrome = ''
      :root {
        --dracula-background: #282a36;
        --dracula-current-line: #44475a;
        --dracula-foreground: #f8f8f2;
        --dracula-comment: #6272a4;
        --dracula-cyan: #8be9fd;
        --dracula-green: #50fa7b;
        --dracula-orange: #ffb86c;
        --dracula-pink: #ff79c6;
        --dracula-purple: #bd93f9;
        --dracula-red: #ff5555;
        --dracula-yellow: #f1fa8c;
        --dracula-black: #21222c;

        --toolbar-bgcolor: var(--dracula-background) !important;
        --toolbar-color: var(--dracula-foreground) !important;
        --toolbar-field-background-color: var(--dracula-current-line) !important;
        --toolbar-field-color: var(--dracula-foreground) !important;
        --toolbar-field-border-color: var(--dracula-comment) !important;
        --toolbar-field-focus-background-color: var(--dracula-black) !important;
        --toolbar-field-focus-border-color: var(--dracula-purple) !important;
        --urlbar-box-bgcolor: var(--dracula-current-line) !important;
        --urlbar-box-text-color: var(--dracula-foreground) !important;
        --urlbar-box-hover-bgcolor: var(--dracula-comment) !important;
        --lwt-sidebar-background-color: var(--dracula-background) !important;
        --lwt-sidebar-text-color: var(--dracula-foreground) !important;
        --arrowpanel-background: var(--dracula-background) !important;
        --arrowpanel-color: var(--dracula-foreground) !important;
        --arrowpanel-border-color: var(--dracula-comment) !important;
        --panel-separator-color: var(--dracula-current-line) !important;
        --autocomplete-popup-background: var(--dracula-background) !important;
        --autocomplete-popup-color: var(--dracula-foreground) !important;
        --autocomplete-popup-highlight-background: var(--dracula-current-line) !important;
        --autocomplete-popup-highlight-color: var(--dracula-cyan) !important;

        --dracula-transition: 300ms cubic-bezier(0.645, 0.045, 0.355, 1);
      }

      #navigator-toolbox {
        background-color: var(--dracula-background) !important;
        border-bottom: 1px solid var(--dracula-current-line) !important;
      }

      #tabbrowser-tabs {
        background-color: var(--dracula-background) !important;
      }

      .tabbrowser-tab {
        background-color: transparent !important;
        transition: all var(--dracula-transition) !important;
      }

      .tabbrowser-tab:hover {
        background-color: var(--dracula-current-line) !important;
      }

      .tabbrowser-tab[selected="true"] {
        background-color: var(--dracula-current-line) !important;
        border-left: 2px solid var(--dracula-purple) !important;
      }

      .tabbrowser-tab .tab-content {
        color: var(--dracula-foreground) !important;
      }

      .tabbrowser-tab:hover .tab-content {
        color: var(--dracula-cyan) !important;
      }

      .tabbrowser-tab[selected="true"] .tab-content {
        color: var(--dracula-purple) !important;
      }

      .tab-close-button {
        fill: var(--dracula-comment) !important;
        transition: fill var(--dracula-transition) !important;
      }

      .tab-close-button:hover {
        fill: var(--dracula-red) !important;
        background-color: transparent !important;
      }

      .tab-line {
        background-color: var(--dracula-purple) !important;
      }

      #nav-bar {
        background-color: var(--dracula-background) !important;
        border-bottom: none !important;
      }

      #urlbar {
        background-color: var(--dracula-current-line) !important;
        color: var(--dracula-foreground) !important;
        border: 1px solid var(--dracula-comment) !important;
        border-radius: 8px !important;
        transition: all var(--dracula-transition) !important;
      }

      #urlbar:hover {
        border-color: var(--dracula-purple) !important;
      }

      #urlbar[focused="true"] {
        background-color: var(--dracula-black) !important;
        border-color: var(--dracula-purple) !important;
        box-shadow: 0 0 0 2px rgba(189, 147, 249, 0.3) !important;
      }

      #urlbar-input {
        color: var(--dracula-foreground) !important;
      }

      #urlbar-container .urlbar-icon,
      #urlbar-container .urlbar-icon-wrapper {
        fill: var(--dracula-comment) !important;
        transition: fill var(--dracula-transition) !important;
      }

      #urlbar-container .urlbar-icon:hover,
      #urlbar-container .urlbar-icon-wrapper:hover {
        fill: var(--dracula-purple) !important;
      }

      toolbarbutton {
        transition: all var(--dracula-transition) !important;
      }

      toolbarbutton:hover {
        background-color: var(--dracula-current-line) !important;
      }

      toolbarbutton .toolbarbutton-icon {
        fill: var(--dracula-foreground) !important;
        transition: fill var(--dracula-transition) !important;
      }

      toolbarbutton:hover .toolbarbutton-icon {
        fill: var(--dracula-cyan) !important;
      }

      #sidebar-box {
        background-color: var(--dracula-background) !important;
        border-right: 1px solid var(--dracula-current-line) !important;
      }

      #sidebar {
        background-color: var(--dracula-background) !important;
        color: var(--dracula-foreground) !important;
      }

      #sidebar-header {
        background-color: var(--dracula-background) !important;
        border-bottom: 1px solid var(--dracula-current-line) !important;
      }

      #sidebar-search-container {
        background-color: var(--dracula-current-line) !important;
        border-radius: 6px !important;
      }

      #sidebar-search-container input {
        background-color: transparent !important;
        color: var(--dracula-foreground) !important;
      }

      #zen-sidebar-top-buttons,
      #zen-sidebar-bottom-buttons {
        background-color: var(--dracula-background) !important;
      }

      .zen-sidebar-action-button {
        fill: var(--dracula-comment) !important;
        transition: all var(--dracula-transition) !important;
      }

      .zen-sidebar-action-button:hover {
        fill: var(--dracula-purple) !important;
        background-color: var(--dracula-current-line) !important;
      }

      #zen-workspaces-button {
        background-color: var(--dracula-current-line) !important;
        border-radius: 6px !important;
        transition: all var(--dracula-transition) !important;
      }

      #zen-workspaces-button:hover {
        background-color: var(--dracula-comment) !important;
      }

      menupopup,
      panel {
        background-color: var(--dracula-background) !important;
        color: var(--dracula-foreground) !important;
        border: 1px solid var(--dracula-current-line) !important;
        border-radius: 8px !important;
      }

      menuitem,
      menu {
        color: var(--dracula-foreground) !important;
        transition: all var(--dracula-transition) !important;
      }

      menuitem:hover,
      menu:hover,
      menuitem[_moz-menuactive="true"],
      menu[_moz-menuactive="true"] {
        background-color: var(--dracula-current-line) !important;
        color: var(--dracula-cyan) !important;
      }

      menuseparator {
        border-color: var(--dracula-current-line) !important;
      }

      #context-navigation {
        background-color: var(--dracula-background) !important;
      }

      findbar {
        background-color: var(--dracula-background) !important;
        border-top: 1px solid var(--dracula-current-line) !important;
      }

      findbar .findbar-textbox {
        background-color: var(--dracula-current-line) !important;
        color: var(--dracula-foreground) !important;
        border: 1px solid var(--dracula-comment) !important;
        border-radius: 4px !important;
      }

      findbar .findbar-textbox:focus {
        border-color: var(--dracula-purple) !important;
      }

      #PopupAutoComplete,
      #PopupSearchAutoComplete {
        background-color: var(--dracula-background) !important;
        border: 1px solid var(--dracula-current-line) !important;
        border-radius: 8px !important;
      }

      .autocomplete-richlistitem {
        background-color: transparent !important;
        color: var(--dracula-foreground) !important;
        transition: all var(--dracula-transition) !important;
      }

      .autocomplete-richlistitem:hover,
      .autocomplete-richlistitem[selected="true"] {
        background-color: var(--dracula-current-line) !important;
        color: var(--dracula-cyan) !important;
      }

      #PersonalToolbar {
        background-color: var(--dracula-background) !important;
        border-bottom: 1px solid var(--dracula-current-line) !important;
      }

      #PlacesToolbarItems toolbarbutton {
        color: var(--dracula-foreground) !important;
        transition: all var(--dracula-transition) !important;
      }

      #PlacesToolbarItems toolbarbutton:hover {
        background-color: var(--dracula-current-line) !important;
        color: var(--dracula-cyan) !important;
      }

      scrollbar {
        background-color: var(--dracula-background) !important;
      }

      scrollbar thumb {
        background-color: var(--dracula-comment) !important;
        border-radius: 4px !important;
      }

      scrollbar thumb:hover {
        background-color: var(--dracula-purple) !important;
      }

      ::selection {
        background-color: var(--dracula-current-line) !important;
        color: var(--dracula-foreground) !important;
      }

      :focus-visible {
        outline: 2px solid var(--dracula-purple) !important;
        outline-offset: 2px !important;
      }
    '';

    userContent = ''
      :root {
        --dracula-background: #282a36;
        --dracula-current-line: #44475a;
        --dracula-foreground: #f8f8f2;
        --dracula-comment: #6272a4;
        --dracula-cyan: #8be9fd;
        --dracula-green: #50fa7b;
        --dracula-orange: #ffb86c;
        --dracula-pink: #ff79c6;
        --dracula-purple: #bd93f9;
        --dracula-red: #ff5555;
        --dracula-yellow: #f1fa8c;
        --dracula-black: #21222c;
      }

      @-moz-document url("about:blank"), url("about:newtab"), url("about:home") {
        body {
          background-color: var(--dracula-background) !important;
          color: var(--dracula-foreground) !important;
        }

        .search-wrapper input,
        .search-handoff-button {
          background-color: var(--dracula-current-line) !important;
          color: var(--dracula-foreground) !important;
          border: 1px solid var(--dracula-comment) !important;
          border-radius: 8px !important;
        }

        .search-wrapper input:focus {
          border-color: var(--dracula-purple) !important;
          box-shadow: 0 0 0 2px rgba(189, 147, 249, 0.3) !important;
        }

        .top-site-outer {
          background-color: transparent !important;
        }

        .top-site-outer:hover {
          background-color: var(--dracula-current-line) !important;
        }

        .top-site-inner {
          background-color: var(--dracula-current-line) !important;
          border-radius: 8px !important;
        }

        .title {
          color: var(--dracula-foreground) !important;
        }

        .card-outer {
          background-color: var(--dracula-current-line) !important;
          border-radius: 8px !important;
        }

        .card-outer:hover {
          box-shadow: 0 0 0 5px var(--dracula-purple) !important;
        }

        .card-title,
        .card-host-name {
          color: var(--dracula-foreground) !important;
        }

        .section-title span {
          color: var(--dracula-comment) !important;
        }
      }

      @-moz-document url-prefix("about:") {
        body,
        #errorPageContainer,
        .container {
          background-color: var(--dracula-background) !important;
          color: var(--dracula-foreground) !important;
        }

        h1, h2, h3, h4, h5, h6 {
          color: var(--dracula-purple) !important;
        }

        a {
          color: var(--dracula-cyan) !important;
        }

        a:hover {
          color: var(--dracula-pink) !important;
        }

        button {
          background-color: var(--dracula-purple) !important;
          color: var(--dracula-foreground) !important;
          border: none !important;
          border-radius: 4px !important;
        }

        button:hover {
          background-color: var(--dracula-pink) !important;
        }

        input,
        textarea {
          background-color: var(--dracula-current-line) !important;
          color: var(--dracula-foreground) !important;
          border: 1px solid var(--dracula-comment) !important;
          border-radius: 4px !important;
        }

        input:focus,
        textarea:focus {
          border-color: var(--dracula-purple) !important;
        }
      }

      * {
        scrollbar-width: thin !important;
        scrollbar-color: var(--dracula-comment) var(--dracula-background) !important;
      }

      ::-webkit-scrollbar {
        width: 10px !important;
        height: 10px !important;
      }

      ::-webkit-scrollbar-track {
        background-color: var(--dracula-background) !important;
      }

      ::-webkit-scrollbar-thumb {
        background-color: var(--dracula-comment) !important;
        border-radius: 5px !important;
        border: 2px solid var(--dracula-background) !important;
      }

      ::-webkit-scrollbar-thumb:hover {
        background-color: var(--dracula-purple) !important;
      }

      ::-webkit-scrollbar-corner {
        background-color: var(--dracula-background) !important;
      }

      ::selection {
        background-color: var(--dracula-current-line) !important;
        color: var(--dracula-foreground) !important;
      }
    '';

    settings = {
      "browser.tabs.warnOnClose" = false;

      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

      "general.smoothScroll" = true;

      "layers.acceleration.force-enabled" = true;

      "browser.uidensity" = 1;

      "extensions.pocket.enabled" = false;
    };
  };
}
