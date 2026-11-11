# Emacs experiments

Never run experiments, repros, or profiling in my live Emacs (the default `emacsclient` socket).
`emacsclient -e` blocks its event loop, freezing the UI, and even light calls switch buffers or
force redisplay. Reading a variable's value is fine.

Instead, use a throwaway daemon and tear it down when done:

    /opt/homebrew/bin/emacs --daemon=scratch
    emacsclient -s scratch --eval '...'

# Emacs binary

Use `/opt/homebrew/bin/emacs`, never bare `emacs`: my shell defines an `emacs` function that runs
`open -a emacs` and `touch`es nonexistent paths into existence.
