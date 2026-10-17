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
# When an installed .elc changes (or is deleted), the file's natively-compiled code in
# var/eln-cache/ is deleted too, so Emacs natively compiles it again from the new .elc. Emacs finds a
# file's .eln by a hash of the file's source, so without this, a file whose source didn't change
# would keep running stale native code, e.g. the old expansion of a macro defined in another file.
#
# An installed .elc gets its file's modification time from before it was compiled. So if the file
# changed during compilation (e.g. a git checkout changed it), it's newer than its .elc, and
# `load-prefer-newer' and elisp-compile.el's startup check treat the .elc as stale.
#
# Usage: compile-elisp.sh [--install] FILE...

set -u
emacs=${EMACS:-/opt/homebrew/bin/emacs}
install=
if [ "${1:-}" = "--install" ]; then
  install=1
  shift
fi
repo_dir=$(cd "$(dirname "$0")/.." && pwd)
elisp_dir=$repo_dir/elisp
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Compile the files in parallel. Each one's output, exit status and .elc go into its own directory.
i=0
for f in "$@"; do
  i=$((i + 1))
  out="$tmp/$i"
  mkdir "$out"
  touch -r "$f" "$out/source-time"
  (
    # - package-load-path.el: the compiler loads each file's `require`d packages, to know their
    #   macros and functions. -Q skips init.el, so the packages must be put on the load-path here.
    # - load-prefer-newer: -Q skips early-init.el, which sets this. Without it, a stale .elc would be
    #   loaded instead of an edited .el.
    # - byte-compile-docstring-max-column: the default is 80, but this repo uses 100 columns.
    "$emacs" -Q --batch \
      -l "$repo_dir/scripts/package-load-path.el" \
      --eval '(setq load-prefer-newer t)' \
      --eval '(setq byte-compile-docstring-max-column 100)' \
      --eval "(setq byte-compile-dest-file-function (lambda (_) \"$out/compiled.elc\"))" \
      -L "$elisp_dir" \
      -f batch-byte-compile "$f" > "$out/log" 2>&1
    echo $? > "$out/status"
  ) &
done
wait

# Deletes the natively-compiled code for the elisp file $1. Its .eln files are named
# <name>-<hash>-<hash>.eln.
delete_eln() {
  local name
  name=$(basename "$1" .el)
  rm -f "$repo_dir"/var/eln-cache/*/"$name"-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f].eln
}

status=0
i=0
for f in "$@"; do
  i=$((i + 1))
  out="$tmp/$i"
  log=$(grep -v '^Loading .* (source)\.\.\.$' "$out/log")
  [ -n "$log" ] && printf '%s\n' "$log"
  if [ "$(cat "$out/status")" -ne 0 ] || printf '%s\n' "$log" | grep -q 'Warning'; then
    status=1
    if [ -n "$install" ] && [ -e "${f}c" ]; then
      rm -f "${f}c"
      delete_eln "$f"
    fi
  elif [ -n "$install" ]; then
    cmp -s "$out/compiled.elc" "${f}c" || delete_eln "$f"
    cp "$out/compiled.elc" "${f}c" && touch -r "$out/source-time" "${f}c" || status=1
  fi
done
exit $status
