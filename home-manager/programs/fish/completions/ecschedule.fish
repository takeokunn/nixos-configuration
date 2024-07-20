# subcommands
complete -f -c ecschedule -n '__fish_use_subcommand' -xa apply -d "apply the rule"
complete -f -c ecschedule -n '__fish_use_subcommand' -xa dump -d "dump tasks"
complete -f -c ecschedule -n '__fish_use_subcommand' -xa run -d "run the rule"
complete -f -c ecschedule -n '__fish_use_subcommand' -xa diff -d "diff of the rule with remote"

# option
complete -c ecschedule -l conf -d "configuration"
complete -c ecschedule -l version -d "display version"
