#!/usr/bin/env bash
# Runs an elisp file in a throwaway GUI Emacs, for experiments which need real redisplay, such as
# profiling scrolling or jit-lock. A daemon or batch Emacs never redisplays.
#
# Usage: scripts/gui-emacs.sh FILE.el [TIMEOUT_SECONDS]
#
# Emacs starts with -Q, so it skips init.el, and with it desktop, the server, recentf and savehist.
# Those would prompt about the live Emacs's lock files, or overwrite its state when quitting.
# Packages and elisp/ are on load-path, so FILE can require what it needs.
#
# FILE runs once the event loop has started. Emacs quits when FILE finishes or signals an error, or
# after TIMEOUT_SECONDS (default 120), without running kill-emacs-hook. The window opens in the
# background, so it doesn't take focus. This prints Emacs's stderr, which holds any error and
# whatever FILE prints with `external-debugging-output'. It exits nonzero unless FILE finished.
set -euo pipefail

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "Usage: $0 FILE.el [TIMEOUT_SECONDS]" >&2
  exit 2
fi

scripts=$(cd "$(dirname "$0")" && pwd)
file=$(realpath "$1")
timeout=${2:-120}
app=/opt/homebrew/opt/emacs-plus@31/Emacs.app
output=$(mktemp -t gui-emacs)
trap 'rm -f "$output"' EXIT

open -g -n -W --stdout "$output" --stderr "$output" -a "$app" --args -Q \
  -l "$scripts/package-load-path.el" -l "$scripts/gui-emacs.el" \
  --eval "(gui-emacs-run \"$file\" $timeout)"

cat "$output"
grep -q '^gui-emacs: done$' "$output"
