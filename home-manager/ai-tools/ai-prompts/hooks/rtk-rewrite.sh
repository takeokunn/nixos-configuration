#!/bin/bash

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

if [[ -n $event ]] && [[ $event != "PreToolUse" ]]; then
  exit 0
fi

if [[ ! -x $RTK_BIN ]]; then
  exit 0
fi

normalized="$(printf '%s' "$command" | perl -0pe 's/\A\s+//; s/\s+\z//')"

nl='
'
case $normalized in
*'|'* | *'&'* | *';'* | *'<'* | *'>'* | *'`'* | *'$('* | *'('* | *')'* | *"$nl"*)
  exit 0
  ;;
esac

set -f
set -- $normalized
set +f
t1="${1:-}"
t2="${2:-}"
t3="${3:-}"

if [[ $t1 == "rtk" ]]; then
  exit 0
fi

rewritten="$("$RTK_BIN" rewrite "$normalized" 2>/dev/null)" || true

if [[ -z $rewritten ]] || [[ $rewritten == "$normalized" ]]; then
  exit 0
fi

allowed=0
case $t1 in
ls | tree | grep | rg | diff)
  allowed=1
  ;;
find)
  case $normalized in
  *' -delete'* | *' -exec'* | *' -ok'* | *' -fprint'* | *' -fls'*) allowed=0 ;;
  *) allowed=1 ;;
  esac
  ;;
git)
  case $t2 in
  status | log | diff | show | blame | describe | shortlog | reflog | ls-files | ls-tree | grep) allowed=1 ;;
  esac
  ;;
gh)
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

case $rewritten in
"rtk "* | *"/rtk "*) : ;;
*) exit 0 ;;
esac

case $rewritten in
*"$nl"*) exit 0 ;;
esac

jq -n --arg cmd "$rewritten" \
  '{hookSpecificOutput: {hookEventName: "PreToolUse", updatedInput: {command: $cmd}}}'
