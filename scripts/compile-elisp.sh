#!/bin/bash
# Byte-compiles each of the given elisp files in its own fresh Emacs, and prints any warnings. Exits
# nonzero if any file has warnings or errors.
#
# Compiling each file on its own means a file must require (or declare) everything it uses. Compiled
# after other files in the same Emacs, a file could instead rely on what they happened to load, e.g.
# a macro from a package it doesn't require. Compiled on its own, such a file compiles the macro call
# as a function call, and its .elc fails when it's loaded.
#
# With --install, each file which compiles without warnings gets its .elc written next to it, and
# each file which doesn't has its .elc deleted, so that a stale .elc is never loaded instead of it.
# Otherwise, the .elc files are written to a temp dir and deleted.
#
# Usage: compile-elisp.sh [--install] FILE...

set -u
emacs=${EMACS:-/opt/homebrew/bin/emacs}
install=
if [ "${1:-}" = "--install" ]; then
  install=1
  shift
fi
elisp_dir=$(cd "$(dirname "$0")/../.emacs.d/elisp" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Compile the files in parallel. Each one's output, exit status and .elc go into its own directory.
i=0
for f in "$@"; do
  i=$((i + 1))
  out="$tmp/$i"
  mkdir "$out"
  (
    # - package-user-dir and package-initialize: the compiler loads each file's `require`d
    #   packages, to know their macros and functions. -Q skips init.el, so the packages must be
    #   activated here.
    # - load-prefer-newer: -Q skips early-init.el, which sets this. Without it, a stale .elc would be
    #   loaded instead of an edited .el.
    # - byte-compile-docstring-max-column: the default is 80, but this repo uses 100 columns.
    "$emacs" -Q --batch \
      --eval '(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))' \
      --eval '(package-initialize)' \
      --eval '(setq load-prefer-newer t)' \
      --eval '(setq byte-compile-docstring-max-column 100)' \
      --eval "(setq byte-compile-dest-file-function (lambda (_) \"$out/compiled.elc\"))" \
      -L "$elisp_dir" \
      -f batch-byte-compile "$f" > "$out/log" 2>&1
    echo $? > "$out/status"
  ) &
done
wait

status=0
i=0
for f in "$@"; do
  i=$((i + 1))
  out="$tmp/$i"
  log=$(grep -v '^Loading .* (source)\.\.\.$' "$out/log")
  [ -n "$log" ] && printf '%s\n' "$log"
  if [ "$(cat "$out/status")" -ne 0 ] || printf '%s\n' "$log" | grep -q 'Warning'; then
    status=1
    [ -n "$install" ] && rm -f "${f}c"
  elif [ -n "$install" ]; then
    cp "$out/compiled.elc" "${f}c" || status=1
  fi
done
exit $status
