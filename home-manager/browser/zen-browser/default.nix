{
  pkgs,
  firefox-addons,
  ...
}:
let
  addons = firefox-addons.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  programs.zen-browser = {
    enable = true;

    # Required for macOS
    darwinDefaultsId = "app.zen-browser.zen";

    policies = {
      DisableAppUpdate = true;
      DisableTelemetry = true;
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
      };
    };

    profiles.default = {
      isDefault = true;

      extensions.packages = with addons; [
        ublock-origin
        vimium
      ];

      userChrome = ''
        /*
         * Dracula Theme for Zen Browser
         * Source: https://draculatheme.com
         * Adapted from: https://github.com/ceuk/firefox-moonlight-dracula
         *
         * Color Palette:
         * Background:    #282a36
         * Current Line:  #44475a
         * Foreground:    #f8f8f2
         * Comment:       #6272a4
         * Cyan:          #8be9fd
         * Green:         #50fa7b
         * Orange:        #ffb86c
         * Pink:          #ff79c6
         * Purple:        #bd93f9
         * Red:           #ff5555
         * Yellow:        #f1fa8c
         * Black:         #21222c
         */

        :root {
          /* Dracula Color Variables */
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

          /* Firefox/Zen Theme Overrides */
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

          /* Transition timing */
          --dracula-transition: 300ms cubic-bezier(0.645, 0.045, 0.355, 1);
        }

        /* ========== Navigator Toolbox ========== */
        #navigator-toolbox {
          background-color: var(--dracula-background) !important;
          border-bottom: 1px solid var(--dracula-current-line) !important;
        }

        /* ========== Tab Bar ========== */
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

        /* Tab line indicator */
        .tab-line {
          background-color: var(--dracula-purple) !important;
        }

        /* ========== Navigation Bar / URL Bar ========== */
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

        /* URL bar buttons */
        #urlbar-container .urlbar-icon,
        #urlbar-container .urlbar-icon-wrapper {
          fill: var(--dracula-comment) !important;
          transition: fill var(--dracula-transition) !important;
        }

        #urlbar-container .urlbar-icon:hover,
        #urlbar-container .urlbar-icon-wrapper:hover {
          fill: var(--dracula-purple) !important;
        }

        /* ========== Toolbar Buttons ========== */
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

        /* ========== Sidebar (Zen Browser specific) ========== */
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

        /* Vertical Tabs (Zen Browser) */
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

        /* Workspaces (Zen Browser) */
        #zen-workspaces-button {
          background-color: var(--dracula-current-line) !important;
          border-radius: 6px !important;
          transition: all var(--dracula-transition) !important;
        }

        #zen-workspaces-button:hover {
          background-color: var(--dracula-comment) !important;
        }

        /* ========== Menus and Popups ========== */
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

        /* Context menu */
        #context-navigation {
          background-color: var(--dracula-background) !important;
        }

        /* ========== Findbar ========== */
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

        /* ========== Autocomplete Popup ========== */
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

        /* ========== Bookmark Bar ========== */
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

        /* ========== Scrollbars in Chrome ========== */
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

        /* ========== Selection ========== */
        ::selection {
          background-color: var(--dracula-current-line) !important;
          color: var(--dracula-foreground) !important;
        }

        /* ========== Focus Outline ========== */
        :focus-visible {
          outline: 2px solid var(--dracula-purple) !important;
          outline-offset: 2px !important;
        }
      '';

      userContent = ''
        /*
         * Dracula Theme for Zen Browser - Content Styles
         * Source: https://draculatheme.com
         * Applies to: New tab page, about: pages, scrollbars
         */

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

        /* ========== New Tab Page ========== */
        @-moz-document url("about:blank"), url("about:newtab"), url("about:home") {
          body {
            background-color: var(--dracula-background) !important;
            color: var(--dracula-foreground) !important;
          }

          /* Search box on new tab */
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

          /* Top sites */
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

          /* Highlights and pocket stories */
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

          /* Section titles */
          .section-title span {
            color: var(--dracula-comment) !important;
          }
        }

        /* ========== About Pages ========== */
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

        /* ========== Scrollbars (All Pages) ========== */
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

        /* ========== Selection ========== */
        ::selection {
          background-color: var(--dracula-current-line) !important;
          color: var(--dracula-foreground) !important;
        }
      '';

      settings = {
        # Disable close warning for cleaner UX (tabs can be restored with Ctrl+Shift+T)
        "browser.tabs.warnOnClose" = false;

        # Enable userChrome.css and userContent.css
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

        # Smooth scrolling
        "general.smoothScroll" = true;

        # Enable GPU acceleration for better CSS performance
        "layers.acceleration.force-enabled" = true;

        # UI density (compact is recommended for Dracula theme)
        "browser.uidensity" = 1;

        # Disable pocket (cleaner new tab)
        "extensions.pocket.enabled" = false;
      };
    };
  };
}
