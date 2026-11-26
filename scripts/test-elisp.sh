#!/bin/bash
# Runs the ERT tests in the given files, each file in its own Emacs, all at once. Each file's output
# is printed whole, in the order the files were given, so the runs' output doesn't interleave. Exits
# nonzero if any file's tests fail.
#
# -Q skips the real init.el, loading only what the test files themselves require. Running each file
# in its own Emacs also means no file's tests depend on what another file loaded.

set -u
emacs=${EMACS:-/opt/homebrew/bin/emacs}
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

pids=()
n=0
for file in "$@"; do
  "$emacs" -Q --batch \
    -l scripts/package-load-path.el \
    --eval '(setq load-prefer-newer t)' \
    -L elisp \
    -l "$file" \
    -f ert-run-tests-batch-and-exit >"$out/$n.log" 2>&1 &
  pids[n]=$!
  n=$((n + 1))
done

failed=""
n=0
for file in "$@"; do
  wait "${pids[n]}" || failed="$failed $file"
  echo "== $file"
  cat "$out/$n.log"
  n=$((n + 1))
done

if [ -n "$failed" ]; then
  echo "Tests failed in:$failed" >&2
  exit 1
fi
