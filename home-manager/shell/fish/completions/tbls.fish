function __tbls_debug
    set -l file "$BASH_COMP_DEBUG_FILE"
    if test -n "$file"
        echo "$argv" >>$file
    end
end

function __tbls_perform_completion
    __tbls_debug "Starting __tbls_perform_completion"

    set -l args (commandline -opc)
    set -l lastArg (string escape -- (commandline -ct))

    __tbls_debug "args: $args"
    __tbls_debug "last arg: $lastArg"

    set -l requestComp "TBLS_ACTIVE_HELP=0 $args[1] __complete $args[2..-1] $lastArg"

    __tbls_debug "Calling $requestComp"
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

    __tbls_debug "Comps: $comps"
    __tbls_debug "DirectiveLine: $directiveLine"
    __tbls_debug "flagPrefix: $flagPrefix"

    for comp in $comps
        printf "%s%s\n" "$flagPrefix" "$comp"
    end

    printf "%s\n" "$directiveLine"
end

function __tbls_perform_completion_once
    __tbls_debug "Starting __tbls_perform_completion_once"

    if test -n "$__tbls_perform_completion_once_result"
        __tbls_debug "Seems like a valid result already exists, skipping __tbls_perform_completion"
        return 0
    end

    set --global __tbls_perform_completion_once_result (__tbls_perform_completion)
    if test -z "$__tbls_perform_completion_once_result"
        __tbls_debug "No completions, probably due to a failure"
        return 1
    end

    __tbls_debug "Performed completions and set __tbls_perform_completion_once_result"
    return 0
end

function __tbls_clear_perform_completion_once_result
    __tbls_debug ""
    __tbls_debug "========= clearing previously set __tbls_perform_completion_once_result variable =========="
    set --erase __tbls_perform_completion_once_result
    __tbls_debug "Successfully erased the variable __tbls_perform_completion_once_result"
end

function __tbls_requires_order_preservation
    __tbls_debug ""
    __tbls_debug "========= checking if order preservation is required =========="

    __tbls_perform_completion_once
    if test -z "$__tbls_perform_completion_once_result"
        __tbls_debug "Error determining if order preservation is required"
        return 1
    end

    set -l directive (string sub --start 2 $__tbls_perform_completion_once_result[-1])
    __tbls_debug "Directive is: $directive"

    set -l shellCompDirectiveKeepOrder 32
    set -l keeporder (math (math --scale 0 $directive / $shellCompDirectiveKeepOrder) % 2)
    __tbls_debug "Keeporder is: $keeporder"

    if test $keeporder -ne 0
        __tbls_debug "This does require order preservation"
        return 0
    end

    __tbls_debug "This doesn't require order preservation"
    return 1
end

function __tbls_prepare_completions
    __tbls_debug ""
    __tbls_debug "========= starting completion logic =========="

    set --erase __tbls_comp_results

    __tbls_perform_completion_once
    __tbls_debug "Completion results: $__tbls_perform_completion_once_result"

    if test -z "$__tbls_perform_completion_once_result"
        __tbls_debug "No completion, probably due to a failure"
        return 1
    end

    set -l directive (string sub --start 2 $__tbls_perform_completion_once_result[-1])
    set --global __tbls_comp_results $__tbls_perform_completion_once_result[1..-2]

    __tbls_debug "Completions are: $__tbls_comp_results"
    __tbls_debug "Directive is: $directive"

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
        __tbls_debug "Received error directive: aborting."
        return 1
    end

    set -l filefilter (math (math --scale 0 $directive / $shellCompDirectiveFilterFileExt) % 2)
    set -l dirfilter (math (math --scale 0 $directive / $shellCompDirectiveFilterDirs) % 2)
    if test $filefilter -eq 1; or test $dirfilter -eq 1
        __tbls_debug "File extension filtering or directory filtering not supported"
        return 1
    end

    set -l nospace (math (math --scale 0 $directive / $shellCompDirectiveNoSpace) % 2)
    set -l nofiles (math (math --scale 0 $directive / $shellCompDirectiveNoFileComp) % 2)

    __tbls_debug "nospace: $nospace, nofiles: $nofiles"

    if test $nospace -ne 0; or test $nofiles -eq 0
        set -l prefix (commandline -t | string escape --style=regex)
        __tbls_debug "prefix: $prefix"

        set -l completions (string match -r -- "^$prefix.*" $__tbls_comp_results)
        set --global __tbls_comp_results $completions
        __tbls_debug "Filtered completions are: $__tbls_comp_results"

        set -l numComps (count $__tbls_comp_results)
        __tbls_debug "numComps: $numComps"

        if test $numComps -eq 1; and test $nospace -ne 0
            set -l split (string split --max 1 \t $__tbls_comp_results[1])

            set -l lastChar (string sub -s -1 -- $split)
            if not string match -r -q "[@=/:.,]" -- "$lastChar"
                __tbls_debug "Adding second completion to perform nospace directive"
                set --global __tbls_comp_results $split[1] $split[1].
                __tbls_debug "Completions are now: $__tbls_comp_results"
            end
        end

        if test $numComps -eq 0; and test $nofiles -eq 0
            __tbls_debug "Requesting file completion"
            return 1
        end
    end

    return 0
end

if type -q tbls
    complete --do-complete "tbls " >/dev/null 2>&1
end

complete -c tbls -e

complete -c tbls -n __tbls_clear_perform_completion_once_result
complete -c tbls -n 'not __tbls_requires_order_preservation && __tbls_prepare_completions' -f -a '$__tbls_comp_results'
complete -k -c tbls -n '__tbls_requires_order_preservation && __tbls_prepare_completions' -f -a '$__tbls_comp_results'
