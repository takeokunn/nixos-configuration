function __copilot_debug
    set -l file "$BASH_COMP_DEBUG_FILE"
    if test -n "$file"
        echo "$argv" >>$file
    end
end

function __copilot_perform_completion
    __copilot_debug "Starting __copilot_perform_completion"

    set -l args (commandline -opc)
    set -l lastArg (string escape -- (commandline -ct))

    __copilot_debug "args: $args"
    __copilot_debug "last arg: $lastArg"

    set -l requestComp "COPILOT_ACTIVE_HELP=0 $args[1] __complete $args[2..-1] $lastArg"

    __copilot_debug "Calling $requestComp"
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

    __copilot_debug "Comps: $comps"
    __copilot_debug "DirectiveLine: $directiveLine"
    __copilot_debug "flagPrefix: $flagPrefix"

    for comp in $comps
        printf "%s%s\n" "$flagPrefix" "$comp"
    end

    printf "%s\n" "$directiveLine"
end

function __copilot_prepare_completions
    __copilot_debug ""
    __copilot_debug "========= starting completion logic =========="

    set --erase __copilot_comp_results

    set -l results (__copilot_perform_completion)
    __copilot_debug "Completion results: $results"

    if test -z "$results"
        __copilot_debug "No completion, probably due to a failure"
        return 1
    end

    set -l directive (string sub --start 2 $results[-1])
    set --global __copilot_comp_results $results[1..-2]

    __copilot_debug "Completions are: $__copilot_comp_results"
    __copilot_debug "Directive is: $directive"

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
        __copilot_debug "Received error directive: aborting."
        return 1
    end

    set -l filefilter (math (math --scale 0 $directive / $shellCompDirectiveFilterFileExt) % 2)
    set -l dirfilter (math (math --scale 0 $directive / $shellCompDirectiveFilterDirs) % 2)
    if test $filefilter -eq 1; or test $dirfilter -eq 1
        __copilot_debug "File extension filtering or directory filtering not supported"
        return 1
    end

    set -l nospace (math (math --scale 0 $directive / $shellCompDirectiveNoSpace) % 2)
    set -l nofiles (math (math --scale 0 $directive / $shellCompDirectiveNoFileComp) % 2)

    __copilot_debug "nospace: $nospace, nofiles: $nofiles"

    if test $nospace -ne 0; or test $nofiles -eq 0
        set -l prefix (commandline -t | string escape --style=regex)
        __copilot_debug "prefix: $prefix"

        set -l completions (string match -r -- "^$prefix.*" $__copilot_comp_results)
        set --global __copilot_comp_results $completions
        __copilot_debug "Filtered completions are: $__copilot_comp_results"

        set -l numComps (count $__copilot_comp_results)
        __copilot_debug "numComps: $numComps"

        if test $numComps -eq 1; and test $nospace -ne 0
            set -l split (string split --max 1 \t $__copilot_comp_results[1])

            set -l lastChar (string sub -s -1 -- $split)
            if not string match -r -q "[@=/:.,]" -- "$lastChar"
                __copilot_debug "Adding second completion to perform nospace directive"
                set --global __copilot_comp_results $split[1] $split[1].
                __copilot_debug "Completions are now: $__copilot_comp_results"
            end
        end

        if test $numComps -eq 0; and test $nofiles -eq 0
            __copilot_debug "Requesting file completion"
            return 1
        end
    end

    return 0
end

if type -q copilot
    complete --do-complete "copilot " >/dev/null 2>&1
end

complete -c copilot -e

complete -c copilot -n __copilot_prepare_completions -f -a '$__copilot_comp_results'
