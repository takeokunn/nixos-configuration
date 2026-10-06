# Tests for the jj-based worktree helpers in ../functions.
# Usage: fish ghq-jj.test.fish <functions_dir>
#
# Needs only fish, git, jj and coreutils. ghq, lsof and fzf are replaced by PATH
# stubs, and every repository is built locally from a local origin.

set -g functions_dir (path resolve -- $argv[1])
test -d "$functions_dir"; or begin
    echo "usage: fish ghq-jj.test.fish <functions_dir>" >&2
    exit 2
end
set fish_function_path $functions_dir $fish_function_path

# Number of check calls this file must execute; a drop means a scenario was
# skipped silently.
set -g expected_checks 122

set -g total 0
set -g failures 0

set -g root (path resolve -- (mktemp -d))
set -g real_jj (command -s jj)

# Nothing may depend on the caller's git or jj configuration.
set -gx HOME $root/home
mkdir -p $HOME
# The state links new_worktree creates (.claude, .serena) are untracked in a
# workspace; a global ignore keeps them out of the snapshot, as on a real
# machine, so a fresh workspace's working-copy commit stays empty. .direnv and
# .env are ignored too, for the `ignored` class of worktree_migrate_jj.
printf '%s\n' .claude .serena .direnv .env >$root/gitignore
printf '[core]\n\texcludesFile = %s\n' $root/gitignore >$root/gitconfig
touch $root/jjconfig
set -gx GIT_CONFIG_GLOBAL $root/gitconfig
set -gx GIT_CONFIG_NOSYSTEM 1
set -gx JJ_CONFIG $root/jjconfig
set -gx JJ_USER tester
set -gx JJ_EMAIL tester@example.com
set -gx GIT_AUTHOR_NAME tester
set -gx GIT_AUTHOR_EMAIL tester@example.com
set -gx GIT_COMMITTER_NAME tester
set -gx GIT_COMMITTER_EMAIL tester@example.com

function check --argument-names desc
    set -g total (math $total + 1)
    if test $argv[2..-1]
        echo "PASS: $desc"
    else
        echo "FAIL: $desc"
        set -g failures (math $failures + 1)
    end
end

# Stubs, installed first on PATH.
set -g stubs $root/stubs
mkdir -p $stubs
printf '%s\n' '#!/bin/sh' 'cat "$GHQ_LIST"' >$stubs/ghq
printf '%s\n' '#!/bin/sh' 'cat >"$FZF_CAPTURE"' >$stubs/fzf
# Call 0 prints LSOF_LIST. From call 1 on it prints LSOF_LIST_2 when that file
# exists, runs LSOF_HOOK, and fails without output when LSOF_FAIL_FROM_1 is set.
printf '%s\n' '#!/bin/sh' \
    '[ "$LSOF_MODE" = fail ] && exit 1' \
    'n=$(cat "$LSOF_COUNTER" 2>/dev/null || echo 0)' \
    'echo $((n + 1)) >"$LSOF_COUNTER"' \
    'list=$LSOF_LIST' \
    'if [ "$n" -ge 1 ]; then' \
    '  [ -n "$LSOF_FAIL_FROM_1" ] && exit 1' \
    '  [ -f "$LSOF_LIST_2" ] && list=$LSOF_LIST_2' \
    '  [ -n "$LSOF_HOOK" ] && eval "$LSOF_HOOK"' \
    fi \
    'echo p1' 'echo fcwd' 'echo n/' \
    '[ -f "$list" ] && while IFS= read -r l; do echo "n$l"; done <"$list"' \
    'exit 0' >$stubs/lsof
# Wraps jj: fails any call that has the word $FAILJJ_ON as an argument, and
# on `sparse` first creates $FAILJJ_MKDIR/.jj holding a marker file (a rival
# init finishing mid-way). Everything else passes through.
set -g failjj $root/failjj
mkdir -p $failjj
printf '%s\n' '#!/bin/sh' \
    'for a; do' \
    '  if [ "$a" = sparse ] && [ -n "$FAILJJ_MKDIR" ]; then mkdir -p "$FAILJJ_MKDIR/.jj"; touch "$FAILJJ_MKDIR/.jj/marker"; fi' \
    '  if [ -n "$FAILJJ_ON" ] && [ "$a" = "$FAILJJ_ON" ]; then echo "forced failure" >&2; exit 1; fi' \
    done \
    "exec $real_jj \"\$@\"" >$failjj/jj
chmod +x $stubs/ghq $stubs/fzf $stubs/lsof $failjj/jj
set -gx PATH $stubs $PATH
set -gx GHQ_LIST $root/ghq.list
set -gx FZF_CAPTURE $root/fzf.capture
set -gx LSOF_LIST $root/lsof.list
set -gx LSOF_LIST_2 $root/lsof.list2
set -gx LSOF_COUNTER $root/lsof.count
touch $GHQ_LIST

function t_commit --argument-names repo parent
    git -C $repo commit-tree (git -C $repo rev-parse "$parent^{tree}") -p $parent -m x
end

function t_init_src --argument-names dir branch
    git init -q -b $branch $dir
    echo hi >$dir/f
    git -C $dir add f
    git -C $dir commit -q -m c1
end

set -g ghq $root/ghq
mkdir -p $ghq

# origin with main, plus feature (ahead of main) and a slashed branch
set -g src $root/src
t_init_src $src main
git -C $src branch feature
git -C $src update-ref refs/heads/feature (t_commit $src feature)
git -C $src branch feat/slash feature
set -g main_commit (git -C $src rev-parse main)

set -g src_master $root/src-master
t_init_src $src_master master
set -g src_both $root/src-both
t_init_src $src_both main
git -C $src_both branch master

function t_clone --argument-names from name
    git clone -q --bare $from $ghq/$name.git
    echo $ghq/$name.git
end

function jj_ws_names --argument-names repo
    jj -R $repo --ignore-working-copy workspace list -T 'name ++ "\n"' 2>/dev/null
end

# ---- C1: bare detection
set -l plain $root/plain
git init -q $plain
mkdir -p $root/fakewt
echo "gitdir: elsewhere" >$root/fakewt/.git
set -l a1 (t_clone $src a1)
__fzf_ghq_bare_p $a1
check "C1 bare clone is bare" $status -eq 0
__fzf_ghq_bare_p $plain
check "C1 normal clone is not bare" $status -ne 0
__fzf_ghq_bare_p $root/fakewt
check "C1 dir with a .git file is not bare" $status -ne 0

# ---- C2: init
set -l out (__fzf_ghq_init_jj $a1 2>&1)
check "C2 init succeeds on a bare clone" $status -eq 0
check "C2 init creates .jj" -d $a1/.jj
check "C2 git_target is ../../.." (cat $a1/.jj/repo/store/git_target) = ../../..
check "C2 git_target has no trailing newline" (wc -c <$a1/.jj/repo/store/git_target | string trim) -eq 8
jj -R $a1 --ignore-working-copy workspace list >/dev/null 2>&1
check "C2 workspace list works" $status -eq 0
check "C2 default workspace is listed" (jj_ws_names $a1 | string join ,) = default
check "C2 no scratch dir left" (count $a1/.jj-init.*) -eq 0
__fzf_ghq_init_jj $a1
check "C2 init is a no-op when .jj exists" $status -eq 0
__fzf_ghq_init_jj $plain 2>/dev/null
check "C2 init refuses a non-bare repo" $status -eq 1
check "C2 refused non-bare repo gets no .jj" ! -e $plain/.jj

# rollback: the sparse step fails; the unpatched jj is the known-good control
set -l a2 (t_clone $src a2)
set -l out (FAILJJ_ON=sparse PATH=$failjj:$PATH __fzf_ghq_init_jj $a2 2>&1)
set -l rc $status
check "C2 rollback: init fails when a step fails" $rc -eq 1
check "C2 rollback: reason is printed with prefix" (string match -q 'fzf_ghq: *forced failure*' -- $out; echo $status) -eq 0
check "C2 rollback: no .jj left" ! -e $a2/.jj
check "C2 rollback: no scratch dir left" (count $a2/.jj-init.*) -eq 0
__fzf_ghq_init_jj $a2
check "C2 control: same repo inits with the real jj" $status -eq 0

# a failure after the move (the final workspace list) still rolls back
set -l a2b (t_clone $src a2b)
set -l out (FAILJJ_ON=workspace PATH=$failjj:$PATH __fzf_ghq_init_jj $a2b 2>&1)
check "C2 late failure: init fails" $status -eq 1
check "C2 late failure: moved .jj is removed" ! -e $a2b/.jj
check "C2 late failure: no scratch dir left" (count $a2b/.jj-init.*) -eq 0

# a rival init that creates .jj before our move must not be touched
set -l a2c (t_clone $src a2c)
set -l out (FAILJJ_MKDIR=$a2c PATH=$failjj:$PATH __fzf_ghq_init_jj $a2c 2>&1)
check "C2 concurrent: init fails" $status -eq 1
check "C2 concurrent: says so" (string match -q "*concurrent jj init*" -- $out; echo $status) -eq 0
check "C2 concurrent: the rival .jj is kept" -f $a2c/.jj/marker
check "C2 concurrent: no scratch dir left" (count $a2c/.jj-init.*) -eq 0
set -l out (__fzf_ghq_init_jj "" 2>&1)
check "C2 empty repo argument fails" $status -eq 1
check "C2 empty repo argument is named" (string match -q "*no repository given*" -- $out; echo $status) -eq 0

# ---- C3/C4: refresh and default ref
__fzf_ghq_jj_refresh $a1 test 2>/dev/null
check "C3 refresh creates main@origin" (jj -R $a1 --ignore-working-copy log --no-graph -r main@origin -T commit_id 2>/dev/null) = $main_commit
check "C4 main@origin is preferred" (__fzf_ghq_resolve_default_ref $a1) = main@origin

set -l a_both (t_clone $src_both both)
__fzf_ghq_init_jj $a_both
__fzf_ghq_jj_refresh $a_both test 2>/dev/null
check "C4 main@origin wins over master@origin" (__fzf_ghq_resolve_default_ref $a_both) = main@origin

set -l a_master (t_clone $src_master master)
__fzf_ghq_init_jj $a_master
__fzf_ghq_jj_refresh $a_master test 2>/dev/null
check "C4 master-only origin resolves master@origin" (__fzf_ghq_resolve_default_ref $a_master) = master@origin

# no origin: the bare HEAD names the local bookmark
set -l loc $ghq/loc.git
git init -q --bare -b main $loc
set -l empty_tree (git -C $loc hash-object -t tree -w --stdin </dev/null)
git -C $loc update-ref refs/heads/main (git -C $loc commit-tree $empty_tree -m c1)
__fzf_ghq_init_jj $loc
__fzf_ghq_jj_refresh $loc test 2>/dev/null
check "C3 refresh without origin imports local refs" (jj -R $loc --ignore-working-copy log --no-graph -r present\(main\) -T commit_id 2>/dev/null | string length) -eq 40
check "C4 no origin falls back to the HEAD branch" (__fzf_ghq_resolve_default_ref $loc) = main

set -l empty $ghq/empty.git
git init -q --bare -b main $empty
__fzf_ghq_init_jj $empty
set -l out (__fzf_ghq_resolve_default_ref $empty)
set -l rc $status
check "C4 unresolvable default fails with status 1" $rc -eq 1
check "C4 unresolvable default prints nothing" -z "$out"

# ---- C5: new worktree
set -l git_wt_before (git -C $a1 worktree list | count)
set -l wt (__fzf_ghq_new_worktree $a1 "" name 2>$root/err)
set -l rc $status
check "C5 creates a worktree from the default ref" $rc -eq 0
check "C5 prints the created path" -d "$wt"
check "C5 worktree is a jj workspace" -f $wt/.jj/repo
check "C5 workspace is checked out (sparse full)" -f $wt/f
check "C5 workspace is registered in jj" (jj_ws_names $a1 | string match -q (path basename $wt); echo $status) -eq 0
check "C5 git worktree list is unchanged" (git -C $a1 worktree list | count) -eq $git_wt_before
check "C5 parent is the default ref commit" (jj -R $wt --ignore-working-copy log --no-graph -r @- -T commit_id) = $main_commit
check "C5 name mode ends in the short sha" (string match -q "*-"(string sub -l 8 -- $main_commit) -- (path basename $wt); echo $status) -eq 0
check "C5 state dir is linked" -L $wt/.claude

set -l wt_b (__fzf_ghq_new_worktree $a1 feature@origin branch 2>/dev/null)
check "C5 branch mode names from the base ref" (string match -q '*-feature' -- (path basename "$wt_b"); echo $status) -eq 0
set -l wt_s (__fzf_ghq_new_worktree $a1 feat/slash@origin branch 2>/dev/null)
check "C5 branch mode maps / to -" (string match -q '*-feat-slash' -- (path basename "$wt_s"); echo $status) -eq 0
check "C5 branch base is the feature commit" (jj -R $wt_b --ignore-working-copy log --no-graph -r @- -T commit_id) = (git -C $src rev-parse feature)

set -l out (__fzf_ghq_new_worktree $a1 'main@origin | feature@origin' name 2>&1)
check "C5 multi-commit base is rejected" $status -eq 1
set -l out (__fzf_ghq_new_worktree $a1 nonexistent@origin name 2>&1)
check "C5 unknown base is rejected" $status -eq 1

set -l out (__fzf_ghq_new_worktree $plain "" name 2>&1)
set -l rc $status
check "C5 non-bare repo without .jj is rejected" $rc -eq 1
check "C5 rejection names the missing jj repository" (string match -q "*has no jj repository*" -- $out; echo $status) -eq 0

# a failed import is an error, on both the default-ref and explicit-ref paths
set -l n_ws (jj_ws_names $a1 | count)
set -l out (FAILJJ_ON=import PATH=$failjj:$PATH __fzf_ghq_new_worktree $a1 "" name 2>&1)
check "C5 import failure with the default ref is an error" $status -eq 1
check "C5 import failure reports itself" (string match -q "*jj git import failed*" -- $out; echo $status) -eq 0
set -l out (FAILJJ_ON=import PATH=$failjj:$PATH __fzf_ghq_new_worktree $a1 main@origin name 2>&1)
check "C5 import failure with an explicit ref is an error" $status -eq 1
check "C5 import failure reports the captured output" (string match -q "*forced failure*" -- $out; echo $status) -eq 0
check "C5 import failure creates no workspace" (jj_ws_names $a1 | count) -eq $n_ws

set -l a3 (t_clone $src a3)
set -l wt_auto (__fzf_ghq_new_worktree $a3 "" name 2>/dev/null)
set -l rc $status
check "C5 bare repo without .jj is initialised on demand" $rc -eq 0
check "C5 auto-init created .jj" -d $a3/.jj
check "C5 auto-init worktree is a jj workspace" -f $wt_auto/.jj/repo

set -l wt_cwd (cd $wt; __fzf_ghq_new_worktree "" "" name 2>/dev/null)
check "C5 empty repo path resolves the root from inside a workspace" (path dirname -- "$wt_cwd") = $a1/.worktrees
set -l out (cd $root; __fzf_ghq_new_worktree "" "" name 2>&1)
set -l rc $status
check "C5 outside any jj repo fails" $rc -eq 1
check "C5 outside any jj repo says so" (string match -q "*not inside a jj repository*" -- $out; echo $status) -eq 0

# ---- C6: worktree_switch repo detection
set -l out (cd $root; worktree_switch 2>&1)
check "C6 switch outside a jj repo fails with its own message" (string match -q "worktree_switch: not inside a jj repository" -- $out; echo $status) -eq 0

# ---- C7: worktree_clean (fzf stub records the candidates and selects none)
set -l c1 (t_clone $src c1)
__fzf_ghq_init_jj $c1
__fzf_ghq_jj_refresh $c1 test 2>/dev/null
echo $c1 >$GHQ_LIST
set -l ws_merged (__fzf_ghq_new_worktree $c1 main@origin name 2>/dev/null)
sleep 1
set -l ws_unmerged (__fzf_ghq_new_worktree $c1 feature@origin name 2>/dev/null)
sleep 1
set -l ws_dirty (__fzf_ghq_new_worktree $c1 main@origin name 2>/dev/null)
check "C7 fixture workspaces exist" -d "$ws_merged" -a -d "$ws_unmerged" -a -d "$ws_dirty"
echo edit >$ws_dirty/new-file
# Aged working-copy commits for the idle signal: one parented on a commit
# outside the base, one on the base itself.
set -l ws_idle (__fzf_ghq_new_worktree $c1 feature@origin name 2>/dev/null)
sleep 1
set -l ws_idle_merged (__fzf_ghq_new_worktree $c1 main@origin name 2>/dev/null)
check "C7 idle fixtures exist" -d "$ws_idle" -a -d "$ws_idle_merged"
for ws in $ws_idle $ws_idle_merged
    JJ_TIMESTAMP=2020-01-01T00:00:00+00:00 jj -R $ws describe -m old >/dev/null 2>&1
end
mkdir -p $root/legacy
git -C $c1 worktree add -q --detach $root/legacy/merged main
git -C $c1 worktree add -q --detach $root/legacy/ahead feature
worktree_clean 2>$root/clean.err
check "C7 worktree_clean exits cleanly" $status -eq 0
set -l captured (cat $FZF_CAPTURE)
set -l tab \t
check "C7 merged jj workspace is offered as merged" (contains -- "merged$tab$ws_merged" $captured; echo $status) -eq 0
check "C7 jj workspace ahead of the base is not a candidate" (string match -q "*$ws_unmerged*" -- $captured; echo $status) -ne 0
check "C7 jj workspace with unsaved edits is not a candidate" (string match -q "*$ws_dirty*" -- $captured; echo $status) -ne 0
check "C7 merged legacy git worktree is offered as merged" (contains -- "merged$tab$root/legacy/merged" $captured; echo $status) -eq 0
check "C7 legacy git worktree ahead of the base is not a candidate" (string match -q "*legacy/ahead*" -- $captured; echo $status) -ne 0
check "C7 old unmerged jj workspace is offered as idle" (contains -- "idle$tab$ws_idle" $captured; echo $status) -eq 0
check "C7 old merged jj workspace is offered as merged,idle" (contains -- "merged,idle$tab$ws_idle_merged" $captured; echo $status) -eq 0
check "C7 exactly four worktrees are offered" (count $captured) -eq 4

# an import failure skips the repo with a note
set -l out (FAILJJ_ON=import PATH=$failjj:$PATH worktree_clean 2>&1)
check "C7 import failure skips the repo with a note" (string match -q "*cannot import refs*skipping*" -- $out; echo $status) -eq 0

# a bare repo without .jj is skipped with a note
echo $a1.nojj >$GHQ_LIST
git clone -q --bare $src $a1.nojj
worktree_clean 2>$root/clean.err
check "C7 bare repo without .jj is skipped with a note" (string match -q "*has no jj repository; skipping*" <$root/clean.err; echo $status) -eq 0

# ---- C9: worktree_rm
set -l r1 (t_clone $src rm1)
__fzf_ghq_init_jj $r1
__fzf_ghq_jj_refresh $r1 test 2>/dev/null
set -l rm_ws (__fzf_ghq_new_worktree $r1 main@origin name 2>/dev/null)
set -l rm_ws2 (__fzf_ghq_new_worktree $r1 main@origin name 2>/dev/null)
pushd $rm_ws >/dev/null
worktree_rm 2>/dev/null
set -l rc $status
set -l after_pwd (pwd -P)
popd >/dev/null
check "C9 worktree_rm removes the current jj workspace" $rc -eq 0 -a ! -e $rm_ws
check "C9 worktree_rm moves to the repo root" "$after_pwd" = $r1
# without a default workspace the repo root is unknown: refuse before removing
jj -R $rm_ws2 workspace forget default >/dev/null 2>&1
pushd $rm_ws2 >/dev/null
set -l out (worktree_rm 2>&1)
set -l rc $status
popd >/dev/null
check "C9 missing default workspace makes worktree_rm fail" $rc -eq 1
check "C9 missing default workspace keeps the workspace" -d $rm_ws2
check "C9 missing default workspace explains itself" (string match -q "*cannot find the default workspace*" -- $out; echo $status) -eq 0

# ---- C10: worktree_migrate_jj
set -l m (t_clone $src mig)
echo $m >$GHQ_LIST
set -l wts $root/wts
mkdir -p $wts
set -l head_main (git -C $m rev-parse main)
set -l orphan (git -C $m commit-tree (git -C $m rev-parse main^{tree}) -p $head_main -m orphan)
function t_wt --argument-names repo path rev
    git -C $repo worktree add -q --detach $path $rev
end
t_wt $m $wts/w10 $head_main
t_wt $m $wts/w1 $head_main
t_wt $m $wts/dirty $head_main
echo x >$wts/dirty/untracked
t_wt $m $wts/unref $orphan
t_wt $m $wts/locked $head_main
git -C $m worktree lock $wts/locked
t_wt $m $wts/gone $head_main
rm -rf $wts/gone
# ignored files that git worktree remove would delete
t_wt $m $wts/ign $head_main
echo SECRET >$wts/ign/.env
# only our own state: links and caches
t_wt $m $wts/state $head_main
ln -s $root/state-target $wts/state/.claude
mkdir -p $wts/state/.serena/memories $wts/state/.direnv
echo x >$wts/state/.serena/memories/note
echo x >$wts/state/.direnv/cache
# combined classes
t_wt $m $wts/lockdirty $head_main
echo x >$wts/lockdirty/untracked
git -C $m worktree lock $wts/lockdirty
t_wt $m $wts/usedirty $head_main
echo x >$wts/usedirty/untracked
t_wt $m $wts/dirtyorph $orphan
echo x >$wts/dirtyorph/untracked
t_wt $m $wts/exact $head_main

# lsof prints resolved paths: w1 and usedirty have a cwd below them, exact has
# one equal to the worktree path, w10 has none (and w1 must not match w10)
printf '%s\n' (path resolve -- $wts/w1)/sub (path resolve -- $wts/usedirty)/deep (path resolve -- $wts/exact) >$LSOF_LIST
set -g dry (worktree_migrate_jj)
set -l rc $status
check "C10 dry-run exits 0" $rc -eq 0
function t_class --argument-names path
    for l in $dry
        string match -q "*"\t"$path" -- $l; and echo (string split -f1 \t -- $l)
    end
end
check "C10 clean worktree is removable" (t_class $wts/w10) = removable
check "C10 dirty (untracked) worktree is dirty" (t_class $wts/dirty) = dirty
check "C10 unreachable HEAD is unreferenced" (t_class $wts/unref) = unreferenced
check "C10 locked worktree is locked" (t_class $wts/locked) = locked
check "C10 cwd below the worktree is in-use (w1 not confused with w10)" (t_class $wts/w1) = in-use
check "C10 cwd equal to the worktree is in-use" (t_class $wts/exact) = in-use
check "C10 missing directory is prunable" (t_class $wts/gone) = prunable
check "C10 ignored .env makes the worktree ignored" (t_class $wts/ign) = ignored
check "C10 .claude, .serena and .direnv alone stay removable" (t_class $wts/state) = removable
check "C10 locked beats dirty" (t_class $wts/lockdirty) = locked
check "C10 in-use beats dirty" (t_class $wts/usedirty) = in-use
check "C10 dirty beats unreferenced" (t_class $wts/dirtyorph) = dirty
check "C10 summary counts removable" (string match -q "removable: 2" -- $dry; echo $status) -eq 0
check "C10 summary counts the other classes" (string match -q "in-use: 3" -- $dry; and string match -q "dirty: 2" -- $dry; and string match -q "prunable: 1" -- $dry; and string match -q "ignored: 1" -- $dry; and string match -q "locked: 2" -- $dry; and string match -q "unreferenced: 1" -- $dry; echo $status) -eq 0
check "C10 dry-run removes nothing" (git -C $m worktree list | count) -eq 13

set -l apply (worktree_migrate_jj --apply 2>/dev/null)
set -l rc $status
check "C10 apply exits 0" $rc -eq 0
check "C10 apply removes the removable worktrees" ! -e $wts/w10 -a ! -e $wts/state
check "C10 apply keeps every other class" -d $wts/dirty -a -d $wts/unref -a -d $wts/locked -a -d $wts/w1 -a -d $wts/ign -a -d $wts/lockdirty -a -d $wts/usedirty -a -d $wts/dirtyorph -a -d $wts/exact
check "C10 apply keeps the ignored file" -f $wts/ign/.env
check "C10 apply prunes the prunable entry" (git -C $m worktree list | string match -q "*$wts/gone*"; echo $status) -ne 0
check "C10 apply reports removed and remaining" (string match -q "*: removed 2, remaining 9" -- $apply; echo $status) -eq 0
check "C10 unreferenced commit is still stored" (git -C $m cat-file -t $orphan) = commit

# fail closed
t_wt $m $wts/w2 $head_main
set -l out (LSOF_MODE=fail worktree_migrate_jj --apply 2>&1)
set -l rc $status
check "C10 failing lsof makes apply return 1" $rc -eq 1
check "C10 failing lsof removes nothing" -d $wts/w2
check "C10 failing lsof prints an error" (string match -q "*cannot detect in-use worktrees*" -- $out; echo $status) -eq 0
mkdir -p $root/nolsof
cp $stubs/ghq $root/nolsof/ghq
set -l out (PATH=$root/nolsof worktree_migrate_jj --apply 2>&1)
set -l rc $status
check "C10 missing lsof makes apply return 1" $rc -eq 1
check "C10 missing lsof removes nothing" -d $wts/w2
check "C10 missing lsof says lsof was not found" (string match -q "*lsof not found*" -- $out; echo $status) -eq 0
rm -f $LSOF_COUNTER
set -l out (LSOF_FAIL_FROM_1=1 worktree_migrate_jj --apply 2>&1)
set -l rc $status
check "C10 a failing rescan before removal returns 1" $rc -eq 1
check "C10 a failing rescan before removal removes nothing" -d $wts/w2
worktree_migrate_jj --bogus 2>/dev/null
check "C10 unknown argument returns 2" $status -eq 2
set -l out (worktree_migrate_jj --bogus 2>&1)
check "C10 usage goes to stderr" (string match -q "usage:*" -- $out; echo $status) -eq 0

# re-checks right before removal, in a fresh repo
set -l m2 (t_clone $src mig2)
echo $m2 >$GHQ_LIST
set -l lc (git -C $m2 commit-tree (git -C $m2 rev-parse main^{tree}) -p main -m late)
git -C $m2 branch tmp $lc
t_wt $m2 $wts/late (git -C $m2 rev-parse main)
t_wt $m2 $wts/lateref $lc
t_wt $m2 $wts/ctl (git -C $m2 rev-parse main)
rm -f $LSOF_COUNTER $LSOF_LIST
# a process enters `late`, and `tmp` disappears, only after the first scan
echo (path resolve -- $wts/late)/x >$LSOF_LIST_2
set -l out (LSOF_HOOK="git -C $m2 update-ref -d refs/heads/tmp" worktree_migrate_jj --apply 2>&1)
set -l rc $status
check "C10 recheck: apply still exits 0" $rc -eq 0
check "C10 recheck: a worktree entered since the scan is kept" -d $wts/late
check "C10 recheck: a worktree whose HEAD lost its ref is kept" -d $wts/lateref
check "C10 recheck: the unchanged worktree is removed" ! -e $wts/ctl
check "C10 recheck: skips are reported" (string match -q "*skipping*late: now in use*" -- $out; and string match -q "*skipping*lateref: HEAD is no longer reachable*" -- $out; echo $status) -eq 0
rm -f $LSOF_LIST_2 $LSOF_COUNTER

# a plain directory nested in another repository is not a bare repo
git -C $plain commit -q --allow-empty -m c
t_wt $plain $root/enclosing HEAD
mkdir -p $plain/nested
echo $plain/nested >$GHQ_LIST
set -l out (worktree_migrate_jj 2>&1)
set -l rc $status
check "C10 nested non-bare dir is skipped with a note" (string match -q "*not a bare repository; skipping*" -- $out; echo $status) -eq 0
check "C10 nested non-bare dir does not expose the enclosing repo" (string match -q "*enclosing*" -- $out; echo $status) -ne 0
check "C10 nested non-bare dir still exits 0" $rc -eq 0

echo "$total checks, $failures failed"
rm -rf -- $root
if test $total -ne $expected_checks
    echo "FAIL: executed $total checks, expected $expected_checks"
    exit 1
end
test $failures -eq 0
