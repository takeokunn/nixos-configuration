#!/bin/bash
# PreToolUse:Bash hook — routes a read-only command through rtk so its output is compressed
# before it reaches the model's context.
#
# Emits only hookSpecificOutput.updatedInput; a permissionDecision would auto-approve the
# command sight-unseen.
#
# rtk's exit status is not a success flag — only stdout (empty, or equal to the input) decides
# whether a rewrite happened.
#
# Not registered in settings.json yet: the emitted command names a bare `rtk`, which would not
# resolve to RTK_BIN's absolute path.

set -euo pipefail

RTK_BIN="@RTK_BIN@"

input=$(cat)

if ! command -v jq &>/dev/null; then
  exit 0
fi

tool_name=$(printf '%s' "$input" | jq -r '.tool_name // ""' 2>/dev/null || echo "")
event=$(printf '%s' "$input" | jq -r '.hook_event_name // ""' 2>/dev/null || echo "")
command=$(printf '%s' "$input" | jq -r '.tool_input.command // ""' 2>/dev/null || echo "")

if [[ $tool_name != "Bash" ]] || [[ -z $command ]]; then
  exit 0
fi

# The field is absent from a hand-fed payload, so only a stated mismatch disqualifies the event.
if [[ -n $event ]] && [[ $event != "PreToolUse" ]]; then
  exit 0
fi

if [[ ! -x $RTK_BIN ]]; then
  exit 0
fi

normalized="$(printf '%s' "$command" | perl -0pe 's/\A\s+//; s/\s+\z//')"

# Exclusion 1: not a lone simple command — inside a pipeline/substitution/redirect the output
# is consumed by another program, not the model, so rewriting there breaks the consumer.
nl='
'
case $normalized in
*'|'* | *'&'* | *';'* | *'<'* | *'>'* | *'`'* | *'$('* | *'('* | *')'* | *"$nl"*)
  exit 0
  ;;
esac

# Globbing is off so an unquoted pattern such as `ls *.nix` splits into tokens rather than
# expanding against whatever directory the hook happens to run in.
set -f
set -- $normalized
set +f
t1="${1:-}"
t2="${2:-}"
t3="${3:-}"

# Exclusion 2: already routed through rtk — an explicit guard for what the equality check below
# would also catch.
if [[ $t1 == "rtk" ]]; then
  exit 0
fi

# rtk decides whether it has a mapping at all. Its stdout is the only signal read here; `|| true`
# keeps set -e from acting on the non-zero status that accompanies a successful rewrite.
rewritten="$("$RTK_BIN" rewrite "$normalized" 2>/dev/null)" || true

if [[ -z $rewritten ]] || [[ $rewritten == "$normalized" ]]; then
  exit 0
fi

# Exclusion 3: only an explicit allowlist of read-only commands may be rewritten — rtk also
# maps commands that mutate state (git push, aws s3 rm, psql DROP TABLE, ...), so "rtk supports
# it" carries no safety of its own. A command survives only if its family is named here and,
# for a family that can also mutate, its sub-command is a reading one.
allowed=0
case $t1 in
ls | tree | grep | rg | diff)
  allowed=1
  ;;
find)
  # find's action flags delete and execute. -exec also covers -execdir, -ok covers -okdir.
  case $normalized in
  *' -delete'* | *' -exec'* | *' -ok'* | *' -fprint'* | *' -fls'*) allowed=0 ;;
  *) allowed=1 ;;
  esac
  ;;
git)
  # branch and tag are omitted: -D and -d make them destructive under the same sub-command.
  case $t2 in
  status | log | diff | show | blame | describe | shortlog | reflog | ls-files | ls-tree | grep) allowed=1 ;;
  esac
  ;;
gh)
  # gh's read/write split sits at the third token, so both are checked. This leaves out `gh api`,
  # which issues whatever HTTP method it is handed.
  case $t2 in
  pr | issue | run | repo | release | workflow)
    case $t3 in
    list | view | status | checks | diff) allowed=1 ;;
    esac
    ;;
  esac
  ;;
docker)
  case $t2 in
  ps | images | logs | top | port | stats | version | info | history | inspect) allowed=1 ;;
  esac
  ;;
kubectl)
  case $t2 in
  get | describe | logs | top | explain | version | api-resources | api-versions | cluster-info) allowed=1 ;;
  esac
  ;;
esac

if [[ $allowed -ne 1 ]]; then
  exit 0
fi

# Nothing downstream re-checks this, so a rewrite that is not a single rtk invocation is dropped
# rather than handed to the tool.
case $rewritten in
"rtk "* | *"/rtk "*) : ;;
*) exit 0 ;;
esac

case $rewritten in
*"$nl"*) exit 0 ;;
esac

jq -n --arg cmd "$rewritten" \
  '{hookSpecificOutput: {hookEventName: "PreToolUse", updatedInput: {command: $cmd}}}'
