### subcommands
complete -f -c zat -n '__fish_use_subcommand' -xa clean -d "Remove app packages in temp folder"
complete -f -c zat -n '__fish_use_subcommand' -xa create -d "Create and install app on your account"
complete -f -c zat -n '__fish_use_subcommand' -xa help -d "Describe available commands or one specific command"
complete -f -c zat -n '__fish_use_subcommand' -xa new -d "Generate a new app"
complete -f -c zat -n '__fish_use_subcommand' -xa package -d "Package your app"
complete -f -c zat -n '__fish_use_subcommand' -xa server -d "Run a http server to serve the local app"
complete -f -c zat -n '__fish_use_subcommand' -xa theme -d "Development tools for Theming Center (Beta)"
complete -f -c zat -n '__fish_use_subcommand' -xa update -d "Update app on the server"
complete -f -c zat -n '__fish_use_subcommand' -xa validate -d "Validate your app"
complete -f -c zat -n '__fish_use_subcommand' -xa version -d "Print the version"

### theme
complete -c zat -n "__fish_seen_subcommand_from theme" -xa help -d "Describe subcommands or one specific subcommand"
complete -c zat -n "__fish_seen_subcommand_from theme" -xa preview -d "Preview a theme in development"
