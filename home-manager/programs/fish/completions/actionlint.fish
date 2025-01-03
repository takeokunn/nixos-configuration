complete -c actionlint -o color -d "Always enable colorful output. This is useful to force colorful outputs"
complete -c actionlint -o config -d "File path to config file"
complete -c actionlint -o debug -d "Enable debug output (for development)"
complete -c actionlint -o format -d "Custom template to format error messages in Go template syntax. See https://github.com/rhysd/actionlint/tree/main/docs/usage.md#format"
complete -c actionlint -o -ignore -d "Regular expression matching to error messages you want to ignore. This flag is repeatable"
complete -c actionlint -o init-config -d "Generate default config file at .github/actionlint.yaml in current project"
complete -c actionlint -o no-color -d "Disable colorful output"
complete -c actionlint -o oneline -d "Use one line per one error. Useful for reading error messages from programs"
complete -c actionlint -o pyflakes -d "Command name or file path of "pyflakes" external command. If empty, pyflakes integration will be disabled (default \"pyflakes\")"
complete -c actionlint -o shellcheck -d "Command name or file path of \"shellcheck\" external command. If empty, shellcheck integration will be disabled (default \"shellcheck\")"
complete -c actionlint -o stdin-filename -d "File name when reading input from stdin"
complete -c actionlint -o verbose -d "Enable verbose output"
complete -c actionlint -o version -d "Show version and how this binary was installed"
