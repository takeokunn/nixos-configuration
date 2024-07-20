# subcommands
complete -f -c tfenv -n '__fish_use_subcommand' -xa install -d "Install a specific version of Terraform"
complete -f -c tfenv -n '__fish_use_subcommand' -xa use -d "Switch a version to use"
complete -f -c tfenv -n '__fish_use_subcommand' -xa uninstall -d "Uninstall a specific version of Terraform"
complete -f -c tfenv -n '__fish_use_subcommand' -xa list -d "List all installed versions"
complete -f -c tfenv -n '__fish_use_subcommand' -xa list-remote -d "List all installable versions"
complete -f -c tfenv -n '__fish_use_subcommand' -xa version-name -d "Print current version"
complete -f -c tfenv -n '__fish_use_subcommand' -xa init -d "Update environment to use tfenv correctly."
complete -f -c tfenv -n '__fish_use_subcommand' -xa pin -d "Write the current active version to ./.terraform-version"
complete -f -c tfenv -n '__fish_use_subcommand' -xa help
