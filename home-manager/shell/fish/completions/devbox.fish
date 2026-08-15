function __devbox_debug
    set -l file "$BASH_COMP_DEBUG_FILE"
    if test -n "$file"
        echo "$argv" >>$file
    end
end

function __devbox_perform_completion
    __devbox_debug "Starting __devbox_perform_completion"

    set -l args (commandline -opc)
    set -l lastArg (string escape -- (commandline -ct))

    __devbox_debug "args: $args"
    __devbox_debug "last arg: $lastArg"

    set -l requestComp "DEVBOX_ACTIVE_HELP=0 $args[1] __complete $args[2..-1] $lastArg"

    __devbox_debug "Calling $requestComp"
    set -l results (eval $requestComp 2> /dev/null)

    for line in $results[-1..1]
        if test (string trim -- $line) = ""
            set results $results[1..-2]
        else
            break
        end
    end

    set -l comps $results[1..-2]
    set -l directiveLine $results[-1]

    set -l flagPrefix (string match -r -- '-.*=' "$lastArg")

    __devbox_debug "Comps: $comps"
    __devbox_debug "DirectiveLine: $directiveLine"
    __devbox_debug "flagPrefix: $flagPrefix"

    for comp in $comps
        printf "%s%s\n" "$flagPrefix" "$comp"
    end

    printf "%s\n" "$directiveLine"
end

function __devbox_perform_completion_once
    __devbox_debug "Starting __devbox_perform_completion_once"

    if test -n "$__devbox_perform_completion_once_result"
        __devbox_debug "Seems like a valid result already exists, skipping __devbox_perform_completion"
        return 0
    end

    set --global __devbox_perform_completion_once_result (__devbox_perform_completion)
    if test -z "$__devbox_perform_completion_once_result"
        __devbox_debug "No completions, probably due to a failure"
        return 1
    end

    __devbox_debug "Performed completions and set __devbox_perform_completion_once_result"
    return 0
end

function __devbox_clear_perform_completion_once_result
    __devbox_debug ""
    __devbox_debug "========= clearing previously set __devbox_perform_completion_once_result variable =========="
    set --erase __devbox_perform_completion_once_result
    __devbox_debug "Successfully erased the variable __devbox_perform_completion_once_result"
end

function __devbox_requires_order_preservation
    __devbox_debug ""
    __devbox_debug "========= checking if order preservation is required =========="

    __devbox_perform_completion_once
    if test -z "$__devbox_perform_completion_once_result"
        __devbox_debug "Error determining if order preservation is required"
        return 1
    end

    set -l directive (string sub --start 2 $__devbox_perform_completion_once_result[-1])
    __devbox_debug "Directive is: $directive"

    set -l shellCompDirectiveKeepOrder 32
    set -l keeporder (math (math --scale 0 $directive / $shellCompDirectiveKeepOrder) % 2)
    __devbox_debug "Keeporder is: $keeporder"

    if test $keeporder -ne 0
        __devbox_debug "This does require order preservation"
        return 0
    end

    __devbox_debug "This doesn't require order preservation"
    return 1
end

function __devbox_prepare_completions
    __devbox_debug ""
    __devbox_debug "========= starting completion logic =========="

    set --erase __devbox_comp_results

    __devbox_perform_completion_once
    __devbox_debug "Completion results: $__devbox_perform_completion_once_result"

    if test -z "$__devbox_perform_completion_once_result"
        __devbox_debug "No completion, probably due to a failure"
        return 1
    end

    set -l directive (string sub --start 2 $__devbox_perform_completion_once_result[-1])
    set --global __devbox_comp_results $__devbox_perform_completion_once_result[1..-2]

    __devbox_debug "Completions are: $__devbox_comp_results"
    __devbox_debug "Directive is: $directive"

    set -l shellCompDirectiveError 1
    set -l shellCompDirectiveNoSpace 2
    set -l shellCompDirectiveNoFileComp 4
    set -l shellCompDirectiveFilterFileExt 8
    set -l shellCompDirectiveFilterDirs 16

    if test -z "$directive"
        set directive 0
    end

    set -l compErr (math (math --scale 0 $directive / $shellCompDirectiveError) % 2)
    if test $compErr -eq 1
        __devbox_debug "Received error directive: aborting."
        return 1
    end

    set -l filefilter (math (math --scale 0 $directive / $shellCompDirectiveFilterFileExt) % 2)
    set -l dirfilter (math (math --scale 0 $directive / $shellCompDirectiveFilterDirs) % 2)
    if test $filefilter -eq 1; or test $dirfilter -eq 1
        __devbox_debug "File extension filtering or directory filtering not supported"
        return 1
    end

    set -l nospace (math (math --scale 0 $directive / $shellCompDirectiveNoSpace) % 2)
    set -l nofiles (math (math --scale 0 $directive / $shellCompDirectiveNoFileComp) % 2)

    __devbox_debug "nospace: $nospace, nofiles: $nofiles"

    if test $nospace -ne 0; or test $nofiles -eq 0
        set -l prefix (commandline -t | string escape --style=regex)
        __devbox_debug "prefix: $prefix"

        set -l completions (string match -r -- "^$prefix.*" $__devbox_comp_results)
        set --global __devbox_comp_results $completions
        __devbox_debug "Filtered completions are: $__devbox_comp_results"

        set -l numComps (count $__devbox_comp_results)
        __devbox_debug "numComps: $numComps"

        if test $numComps -eq 1; and test $nospace -ne 0
            set -l split (string split --max 1 \t $__devbox_comp_results[1])

            set -l lastChar (string sub -s -1 -- $split)
            if not string match -r -q "[@=/:.,]" -- "$lastChar"
                __devbox_debug "Adding second completion to perform nospace directive"
                set --global __devbox_comp_results $split[1] $split[1].
                __devbox_debug "Completions are now: $__devbox_comp_results"
            end
        end

        if test $numComps -eq 0; and test $nofiles -eq 0
            __devbox_debug "Requesting file completion"
            return 1
        end
    end

    return 0
end

if type -q devbox
    complete --do-complete "devbox " >/dev/null 2>&1
end

complete -c devbox -e

complete -c devbox -n __devbox_clear_perform_completion_once_result
complete -c devbox -n 'not __devbox_requires_order_preservation && __devbox_prepare_completions' -f -a '$__devbox_comp_results'
complete -k -c devbox -n '__devbox_requires_order_preservation && __devbox_prepare_completions' -f -a '$__devbox_comp_results'
