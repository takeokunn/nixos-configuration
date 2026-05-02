# option
complete -c cmigemo -s d -l dict -d "Use a file <dict> for dictionary."
complete -c cmigemo -s s -l subdict -d "Sub dictionary files. (MAX 8 times)"
complete -c cmigemo -s q -l quiet -d "Show no message except results."
complete -c cmigemo -s v -l vim -d "Use vim style regexp."
complete -c cmigemo -s e -l emacs -d "Use emacs style regexp."
complete -c cmigemo -s n -l nonewline -d "Don't use newline match."
complete -c cmigemo -s w -l word -d "Expand a <word> and soon exit."
complete -c cmigemo -s h -l help -d "Show this message."
