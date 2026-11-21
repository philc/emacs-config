# Emacs experiments

Never run experiments, repros, or profiling in my live Emacs (the default `emacsclient` socket).
`emacsclient -e` blocks its event loop, freezing the UI, and even light calls switch buffers or
force redisplay. Reading a variable's value is fine.

Instead, use a throwaway daemon and tear it down when done:

    /opt/homebrew/bin/emacs --daemon=scratch
    emacsclient -s scratch --eval '...'

A daemon never redisplays, so it can't reproduce problems with scrolling, jit-lock, or anything else
that runs during redisplay. For those, write the experiment as an elisp file and run it with
`scripts/gui-emacs.sh FILE.el`. It runs a -Q GUI Emacs in the background, then quits without saving
any state. Don't launch GUI Emacs any other way. A full init.el in a second Emacs prompts about my
desktop lock and can overwrite my desktop, recentf and savehist files.

# Emacs binary

Use `/opt/homebrew/bin/emacs`, never bare `emacs`: my shell defines an `emacs` function that runs
`open -a emacs` and `touch`es nonexistent paths into existence.
