function __rain_debug
    set -l file "$BASH_COMP_DEBUG_FILE"
    if test -n "$file"
        echo "$argv" >>$file
    end
end

function __rain_perform_completion
    __rain_debug "Starting __rain_perform_completion"

    set -l args (commandline -opc)
    set -l lastArg (string escape -- (commandline -ct))

    __rain_debug "args: $args"
    __rain_debug "last arg: $lastArg"

    set -l requestComp "RAIN_ACTIVE_HELP=0 $args[1] __complete $args[2..-1] $lastArg"

    __rain_debug "Calling $requestComp"
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

    __rain_debug "Comps: $comps"
    __rain_debug "DirectiveLine: $directiveLine"
    __rain_debug "flagPrefix: $flagPrefix"

    for comp in $comps
        printf "%s%s\n" "$flagPrefix" "$comp"
    end

    printf "%s\n" "$directiveLine"
end

function __rain_perform_completion_once
    __rain_debug "Starting __rain_perform_completion_once"

    if test -n "$__rain_perform_completion_once_result"
        __rain_debug "Seems like a valid result already exists, skipping __rain_perform_completion"
        return 0
    end

    set --global __rain_perform_completion_once_result (__rain_perform_completion)
    if test -z "$__rain_perform_completion_once_result"
        __rain_debug "No completions, probably due to a failure"
        return 1
    end

    __rain_debug "Performed completions and set __rain_perform_completion_once_result"
    return 0
end

function __rain_clear_perform_completion_once_result
    __rain_debug ""
    __rain_debug "========= clearing previously set __rain_perform_completion_once_result variable =========="
    set --erase __rain_perform_completion_once_result
    __rain_debug "Successfully erased the variable __rain_perform_completion_once_result"
end

function __rain_requires_order_preservation
    __rain_debug ""
    __rain_debug "========= checking if order preservation is required =========="

    __rain_perform_completion_once
    if test -z "$__rain_perform_completion_once_result"
        __rain_debug "Error determining if order preservation is required"
        return 1
    end

    set -l directive (string sub --start 2 $__rain_perform_completion_once_result[-1])
    __rain_debug "Directive is: $directive"

    set -l shellCompDirectiveKeepOrder 32
    set -l keeporder (math (math --scale 0 $directive / $shellCompDirectiveKeepOrder) % 2)
    __rain_debug "Keeporder is: $keeporder"

    if test $keeporder -ne 0
        __rain_debug "This does require order preservation"
        return 0
    end

    __rain_debug "This doesn't require order preservation"
    return 1
end

function __rain_prepare_completions
    __rain_debug ""
    __rain_debug "========= starting completion logic =========="

    set --erase __rain_comp_results

    __rain_perform_completion_once
    __rain_debug "Completion results: $__rain_perform_completion_once_result"

    if test -z "$__rain_perform_completion_once_result"
        __rain_debug "No completion, probably due to a failure"
        return 1
    end

    set -l directive (string sub --start 2 $__rain_perform_completion_once_result[-1])
    set --global __rain_comp_results $__rain_perform_completion_once_result[1..-2]

    __rain_debug "Completions are: $__rain_comp_results"
    __rain_debug "Directive is: $directive"

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
        __rain_debug "Received error directive: aborting."
        return 1
    end

    set -l filefilter (math (math --scale 0 $directive / $shellCompDirectiveFilterFileExt) % 2)
    set -l dirfilter (math (math --scale 0 $directive / $shellCompDirectiveFilterDirs) % 2)
    if test $filefilter -eq 1; or test $dirfilter -eq 1
        __rain_debug "File extension filtering or directory filtering not supported"
        return 1
    end

    set -l nospace (math (math --scale 0 $directive / $shellCompDirectiveNoSpace) % 2)
    set -l nofiles (math (math --scale 0 $directive / $shellCompDirectiveNoFileComp) % 2)

    __rain_debug "nospace: $nospace, nofiles: $nofiles"

    if test $nospace -ne 0; or test $nofiles -eq 0
        set -l prefix (commandline -t | string escape --style=regex)
        __rain_debug "prefix: $prefix"

        set -l completions (string match -r -- "^$prefix.*" $__rain_comp_results)
        set --global __rain_comp_results $completions
        __rain_debug "Filtered completions are: $__rain_comp_results"

        set -l numComps (count $__rain_comp_results)
        __rain_debug "numComps: $numComps"

        if test $numComps -eq 1; and test $nospace -ne 0
            set -l split (string split --max 1 \t $__rain_comp_results[1])

            set -l lastChar (string sub -s -1 -- $split)
            if not string match -r -q "[@=/:.,]" -- "$lastChar"
                __rain_debug "Adding second completion to perform nospace directive"
                set --global __rain_comp_results $split[1] $split[1].
                __rain_debug "Completions are now: $__rain_comp_results"
            end
        end

        if test $numComps -eq 0; and test $nofiles -eq 0
            __rain_debug "Requesting file completion"
            return 1
        end
    end

    return 0
end

if type -q rain
    complete --do-complete "rain " >/dev/null 2>&1
end

complete -c rain -e

complete -c rain -n __rain_clear_perform_completion_once_result
complete -c rain -n 'not __rain_requires_order_preservation && __rain_prepare_completions' -f -a '$__rain_comp_results'
complete -k -c rain -n '__rain_requires_order_preservation && __rain_prepare_completions' -f -a '$__rain_comp_results'
