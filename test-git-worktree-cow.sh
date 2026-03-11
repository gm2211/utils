#!/usr/bin/env bash
#
# Integration tests for git-worktree-cow
#
set -euo pipefail

PASS=0
FAIL=0
TMPDIR_BASE=""

cleanup() {
    if [[ -n "$TMPDIR_BASE" ]] && [[ -d "$TMPDIR_BASE" ]]; then
        rm -rf "$TMPDIR_BASE"
    fi
}
trap cleanup EXIT

ok()   { PASS=$((PASS + 1)); printf '  \033[32m✓\033[0m %s\n' "$1"; }
fail() { FAIL=$((FAIL + 1)); printf '  \033[31m✗\033[0m %s: %s\n' "$1" "$2"; }

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GIT_WORKTREE_COW="$SCRIPT_DIR/git-worktree-cow"

# Make the script available as "git worktree-cow"
export PATH="$SCRIPT_DIR:$PATH"

# ── Setup: create a test repo ────────────────────────────────────────────────

TMPDIR_BASE="$(mktemp -d)"
REPO="$TMPDIR_BASE/repo"

mkdir -p "$REPO"
git -C "$REPO" init -b main >/dev/null 2>&1
git -C "$REPO" config user.email "test@test.com"
git -C "$REPO" config user.name "Test"
git -C "$REPO" config commit.gpgsign false

echo "hello world" > "$REPO/file1.txt"
mkdir -p "$REPO/subdir"
echo "nested file" > "$REPO/subdir/file2.txt"
git -C "$REPO" add -A
git -C "$REPO" commit -m "initial commit" >/dev/null 2>&1

echo "second change" >> "$REPO/file1.txt"
git -C "$REPO" add -A
git -C "$REPO" commit -m "second commit" >/dev/null 2>&1

printf '\n══ Test Suite: git worktree-cow ══\n\n'

# ── Test 1: Basic add ────────────────────────────────────────────────────────
printf 'Test 1: basic add (same branch)\n'
WT1="$TMPDIR_BASE/wt1"
(cd "$REPO" && git worktree-cow add "$WT1") >/dev/null 2>&1

if [[ -f "$WT1/file1.txt" ]]; then
    ok "files copied"
else
    fail "files copied" "file1.txt missing"
fi

if [[ -f "$WT1/.git" ]] && grep -q "gitdir:" "$WT1/.git"; then
    ok ".git file points to admin dir"
else
    fail ".git file" "not a gitdir link"
fi

if git -C "$WT1" status >/dev/null 2>&1; then
    ok "git status works in new worktree"
else
    fail "git status" "command failed"
fi

if [[ -f "$WT1/subdir/file2.txt" ]]; then
    ok "nested files copied"
else
    fail "nested files" "subdir/file2.txt missing"
fi

# Verify content matches source
if diff -q "$REPO/file1.txt" "$WT1/file1.txt" >/dev/null 2>&1; then
    ok "file content matches source"
else
    fail "file content" "content differs"
fi

# ── Test 2: Add with new branch ─────────────────────────────────────────────
printf '\nTest 2: add with -b <new-branch>\n'
WT2="$TMPDIR_BASE/wt2"
(cd "$REPO" && git worktree-cow add -b feature-1 "$WT2") >/dev/null 2>&1

branch="$(git -C "$WT2" symbolic-ref --short HEAD 2>/dev/null || echo "")"
if [[ "$branch" == "feature-1" ]]; then
    ok "new branch created and checked out"
else
    fail "branch" "expected feature-1, got '$branch'"
fi

# ── Test 3: Add at specific commit ──────────────────────────────────────────
printf '\nTest 3: add at specific commit (detached HEAD)\n'
WT3="$TMPDIR_BASE/wt3"
first_commit="$(git -C "$REPO" rev-list --max-parents=0 HEAD)"
(cd "$REPO" && git worktree-cow add --detach "$WT3" "$first_commit") >/dev/null 2>&1

wt3_head="$(git -C "$WT3" rev-parse HEAD 2>/dev/null || echo "")"
if [[ "$wt3_head" == "$first_commit" ]]; then
    ok "HEAD points to specified commit"
else
    fail "HEAD" "expected $first_commit, got $wt3_head"
fi

# ── Test 4: List shows all worktrees ─────────────────────────────────────────
printf '\nTest 4: list shows cow worktrees\n'
list_output="$(git -C "$REPO" worktree list 2>/dev/null)"
if echo "$list_output" | grep -q "wt1"; then
    ok "wt1 appears in worktree list"
else
    fail "list" "wt1 not in list output"
fi

# ── Test 5: Changes in worktree are independent ─────────────────────────────
printf '\nTest 5: changes are independent (CoW semantics)\n'
echo "worktree-only change" >> "$WT1/file1.txt"

if ! diff -q "$REPO/file1.txt" "$WT1/file1.txt" >/dev/null 2>&1; then
    ok "changes in worktree don't affect source"
else
    fail "independence" "files are still identical after modification"
fi

# ── Test 6: Lock support ────────────────────────────────────────────────────
printf '\nTest 6: --lock flag\n'
WT6="$TMPDIR_BASE/wt6"
(cd "$REPO" && git worktree-cow add --lock --reason "testing" "$WT6") >/dev/null 2>&1

lock_output="$(git -C "$REPO" worktree list --porcelain 2>/dev/null)"
if echo "$lock_output" | grep -q "locked"; then
    ok "worktree is locked"
else
    fail "lock" "worktree not locked"
fi

# ── Test 7: Remove worktree ─────────────────────────────────────────────────
printf '\nTest 7: remove (passthrough)\n'
(cd "$REPO" && git worktree unlock "$WT6" 2>/dev/null || true)
if (cd "$REPO" && git worktree-cow remove "$WT6") >/dev/null 2>&1; then
    if [[ ! -d "$WT6" ]]; then
        ok "worktree removed"
    else
        fail "remove" "directory still exists"
    fi
else
    fail "remove" "command failed"
fi

# ── Test 8: Error on existing path without -f ────────────────────────────────
printf '\nTest 8: error on existing path without -f\n'
mkdir -p "$TMPDIR_BASE/existing"
if (cd "$REPO" && git worktree-cow add "$TMPDIR_BASE/existing") >/dev/null 2>&1; then
    fail "existing path" "should have failed"
else
    ok "rejects existing path without -f"
fi

# ── Test 9: -f overrides existing path ───────────────────────────────────────
printf '\nTest 9: -f allows overwriting existing path\n'
if (cd "$REPO" && git worktree-cow add -f "$TMPDIR_BASE/existing") >/dev/null 2>&1; then
    ok "-f allows overwrite"
else
    fail "-f overwrite" "command failed"
fi

# ── Test 10: help flag ───────────────────────────────────────────────────────
printf '\nTest 10: --help\n'
if "$GIT_WORKTREE_COW" --help 2>&1 | grep -q "copy-on-write"; then
    ok "--help shows usage"
else
    fail "--help" "no usage output"
fi

# ── Summary ──────────────────────────────────────────────────────────────────
printf '\n══ Results: %d passed, %d failed ══\n\n' "$PASS" "$FAIL"
[[ "$FAIL" -eq 0 ]] || exit 1
