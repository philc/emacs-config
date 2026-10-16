# emacs-config

My Emacs configuration. `~/.emacs.d` is a symlink to this repo, so `init.el` and `early-init.el` at
the root are what Emacs loads. Personal elisp lives in `elisp/`.

Runtime state like history, caches, and installed packages goes in `var/` and `etc/`, which are
gitignored.

## Setting up a new machine

TODO(philc): This step 1 is specific. Just mention that there are some scripts written in Javascript
that require Deno.

1. Install Emacs and the tools the config depends on:

   ```sh
   brew tap d12frosted/emacs-plus
   brew install emacs-plus@31
   brew install git enchant pkgconf the_silver_searcher deno
   xcode-select --install # A C compiler, for jinx's module and the tree-sitter grammars
   ```

   enchant and pkgconf are needed to compile jinx (the spell checker). the_silver_searcher is used by
   `ag`. deno runs the scripts in `scripts/`.

TODO(philc): Remove this; too obvious.
2. Clone the repo and link it as `~/.emacs.d`:

   ```sh
   git clone git@github.com:philc/emacs-config.git ~/projects/emacs-config
   ln -s projects/emacs-config ~/.emacs.d
   ```

3. Install the packages, at the versions in `package-lock.el`:

   ```sh
   cd ~/projects/emacs-config
   make sync-packages
   ```

   This step is optional: starting Emacs installs any missing packages too. But doing it first shows
   any errors in the terminal, rather than during Emacs's startup. It needs network access to GitHub.

4. Start Emacs. On first start it also compiles `elisp/` and installs tree-sitter grammars.

## Packages

Packages are installed with [straight.el](https://github.com/radian-software/straight.el) rather than
package.el. straight.el clones each package's git repo into `var/straight/repos/` and builds it into
`var/straight/build/`. The packages to install are listed in `my-packages` in `packages.el`, and
their dependencies are installed automatically.

### Pinned versions

Every package is pinned to a specific commit, recorded in `package-lock.el`, which is checked in.
When a package is installed, its pinned commit is checked out, so every machine runs the same code,
and an upgrade can be undone by reverting `package-lock.el`. package.el can't do this: package
archives only serve each package's latest version.

When upgrading, packages move to their newest release (i.e. their newest version tag), like MELPA
Stable, rather than to the newest commit. Packages whose last release is years old, listed in
`my-packages-at-head` in `packages.el`, and packages which don't tag releases, move to the newest
commit on their default branch instead.

### Commands

| Command | What it does |
|---|---|
| `make sync-packages` | Installs missing packages, and checks out the versions in `package-lock.el`. |
| `make upgrade-packages` | Moves each package to its newest release (or commit) |
| `make package-versions` | Prints the installed version of each package. |

To add a package, add it to `my-packages`, run `make sync-packages`, then `make upgrade-packages` to
move it to its latest release. To roll back an upgrade, revert `package-lock.el` and run `make
sync-packages`.

## Development

Run `make help`.
