#!/usr/bin/env python3
"""Exclude literal search patterns from credential-path checks, not from execution."""

import json
import re
import shlex
import subprocess
import sys


# Unknown options keep the original event: their next argument might be a file.
SHORT_FLAGS = {
    "rg": set("iSsvwxUFPLauznNHIlcq"),
    "grep": set("EFGPivwxzclLnHhoqsrRaIUbZ"),
}
SHORT_VALUES = {"rg": set("efgjtTABCEMm"), "grep": set("efABCmdD")}
LONG_FLAGS = {
    "rg": set("ignore-case smart-case case-sensitive invert-match word-regexp line-regexp "
              "fixed-strings pcre2 multiline multiline-dotall text hidden no-ignore "
              "no-ignore-vcs no-ignore-parent follow line-number no-line-number with-filename "
              "no-filename files-with-matches files-without-match count count-matches quiet "
              "only-matching null null-data no-messages".split()),
    "grep": set("extended-regexp fixed-strings basic-regexp perl-regexp ignore-case "
                "no-ignore-case invert-match word-regexp line-regexp null-data count "
                "files-with-matches files-without-match line-number with-filename "
                "no-filename only-matching quiet silent no-messages recursive "
                "dereference-recursive text binary byte-offset null".split()),
}
LONG_VALUES = {
    "rg": set("regexp file glob iglob type type-not threads after-context before-context "
              "context max-count max-columns max-depth encoding color colors sort sortr "
              "replace path-separator field-match-separator field-context-separator "
              "ignore-file type-add type-clear engine".split()),
    "grep": set("regexp file after-context before-context context max-count "
                "directories devices binary-files include exclude exclude-dir "
                "exclude-from label".split()),
}


def search_command(command):
    # No shell execution, expansion, pipelines, compound commands, or heredocs are interpreted.
    if any(char in command for char in "$`\n\r"):
        return command
    lexer = shlex.shlex(command, posix=True, punctuation_chars="();<>|&")
    lexer.whitespace_split = True
    lexer.commenters = ""
    try:
        words = list(lexer)
    except ValueError:
        return command
    if not words or words[0] not in SHORT_FLAGS:
        return command
    tool = words[0]
    indices = []
    index = 1
    while index < len(words):
        word = words[index]
        if word in ("<", ">", ">>"):
            index += 1
            if index == len(words) or not words[index] or any(c in words[index] for c in "();<>|&"):
                return command
        elif any(c in word for c in "();<>|&"):
            return command
        else:
            indices.append(index)
        index += 1

    patterns = []
    positional = []
    explicit = False
    options = True
    cursor = 0
    while cursor < len(indices):
        index = indices[cursor]
        word = words[index]
        pattern_offset = None
        consumes_value = False
        option = ""
        if options and word == "--":
            options = False
        elif options and word.startswith("--"):
            option, equal, value = word[2:].partition("=")
            if option in LONG_FLAGS[tool] and not equal:
                pass
            elif option in LONG_VALUES[tool]:
                consumes_value = not equal
                if equal and option == "regexp":
                    pattern_offset = len(word) - len(value)
            else:
                return command
            explicit |= option in ("regexp", "file")
        elif options and word.startswith("-") and word != "-":
            for offset, letter in enumerate(word[1:], 1):
                if letter in SHORT_VALUES[tool]:
                    option = {"e": "regexp", "f": "file"}.get(letter, letter)
                    explicit |= letter in "ef"
                    consumes_value = offset == len(word) - 1
                    if not consumes_value and letter == "e":
                        pattern_offset = offset + 1
                    break
                if letter not in SHORT_FLAGS[tool]:
                    return command
            else:
                option = ""
        else:
            positional.append(index)

        if consumes_value:
            cursor += 1
            if cursor == len(indices):
                return command
            if option == "regexp":
                patterns.append((indices[cursor], 0))
        elif pattern_offset is not None:
            patterns.append((index, pattern_offset))
        cursor += 1

    if not explicit and positional:
        patterns.append((positional[0], 0))
    if not patterns:
        return command
    changed = False
    for index, offset in patterns:
        # Only the ambiguous identifier.env spelling is exempted. Existing .env,
        # .env.* and key-name rules still inspect patterns, as well as every path.
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)*\.env", words[index][offset:]):
            words[index] = words[index][:offset] + "GUARD_SEARCH_PATTERN"
            changed = True
    if not changed:
        return command
    return shlex.join(words)


def main():
    raw = sys.stdin.buffer.read()
    try:
        event = json.loads(raw)
        if not isinstance(event, dict):
            raise ValueError
        if event.get("tool_name") == "Bash":
            tool_input = event.get("tool_input")
            if not isinstance(tool_input, dict) or not isinstance(tool_input.get("command"), str):
                raise ValueError
            command = tool_input["command"]
            filtered = search_command(command)
            if filtered != command:
                tool_input["command"] = filtered
                raw = json.dumps(event, ensure_ascii=True).encode()
    except (ValueError, UnicodeError, RecursionError):
        print("Credential guard input is not a valid event.", file=sys.stderr)
        return 2
    if len(sys.argv) < 2:
        print("Credential guard executable is missing.", file=sys.stderr)
        return 2
    try:
        result = subprocess.run(sys.argv[1:], input=raw, check=False)
    except OSError:
        print("Credential guard could not start.", file=sys.stderr)
        return 2
    return result.returncode if result.returncode >= 0 else 2


if __name__ == "__main__":
    sys.exit(main())
