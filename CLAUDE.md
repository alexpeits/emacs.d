# Emacs configuration

Personal Emacs config, currently run as GNU Emacs 31.1 (Emacs.app on macOS, also
used with `-nw`). Packages come from GNU ELPA + MELPA via `package.el` and
`use-package` (built in). The user hadn't used Emacs in a while before the
October 2026 cleanup, so the config carries older layers.

## Layout and load order

1. `early-init.el`: only `frame-inhibit-implied-resize`. package.el
   activates packages itself (`package-enable-at-startup` is left at `t`).
2. `init.el`: GC / `file-name-handler-alist` startup tweaks, then
   `(require 'org)` and `org-babel-load-file` on `configuration.org`.
3. `configuration.org`: **the whole config**, a literate org file. It is
   tangled to `configuration.el` (gitignored) on startup whenever the org file is
   newer. `README.org` is a symlink to it. Blocks with `:tangle no` are not loaded.
4. Near the end, `configuration.org` loads `hosts/init-<hostname>.el` (from the
   `hostname` command, not `system-name`). A missing host file only produces a
   warning. Host files set `my/fonts`, `my/font-variant`, `my/avail-themes`,
   `my/current-theme`, `magit-repository-directories`, and projectile name
   overrides. `hosts/.gitignore` ignores everything except
   `init-seabeast.el` and `init-Alexandross-MacBook-Pro.local.el`; the current
   machine's file (`init-OG-MacBookPro-Setup-C008022F.el`) is untracked.
5. `lisp/` is on `load-path` (local packages such as `term-cursor`,
   `flycheck-vale`, `ox-confluence`). `custom-themes/` is on
   `custom-theme-load-path` (`monad`, `functor`, `predawn`, `foldable`).

Generated and state files (`elpa/`, `eln-cache/`, `custom.el`, `recentf`,
`projects`, `transient/`, `tree-sitter/`, `undo/` ...) are gitignored.

## Conventions

- Everything is `lexical-binding: t`, including host files. If a function
  `let`-binds another package's variable and may run before that package
  loads, add a top-level `(defvar the-var)` first. Without it the binding is
  lexical and the package never sees it. Existing examples:
  `edit-indirect-guess-mode-function`, `eshell-banner-message`,
  `projectile-switch-project-action`, the `org-export-*`/`org-html-*` vars,
  `consult-ripgrep-args`.
- Personal definitions use the `my/` prefix. `my/add-hooks` adds the same body
  to several hooks. `my/define-major-mode-key` binds per-mode keys under
  `<C-m>` (GUI) or `C-c m m` (terminal), and also under evil-leader `SPC m`.
- Leader keys live in the `my/leader-keys` list. They are bound on `C-c m`
  (and `C-c <C-m>` in the GUI) and on evil-leader `SPC`.
- Evil is **per buffer** (`evil-local-mode` from prog/text/conf mode hooks),
  not global. `<f5>` toggles it. Undo system: built-in `undo-redo`.
- `use-package` `:mode` entries are prepended, so **list more specific patterns
  last** (see the ruby block: `_spec.rb` comes after `.rb`).
- The user wants **existing keybindings preserved** when packages change. Rebind
  the same keys to the replacement commands, and say plainly if a key has to
  move.
- Don't add `:tangle no` blocks or commented-out code for removed features.
  Delete them; git has the history.

## Completion stack (switched from ivy/counsel/swiper + company, 2026-10)

The "completion (vertico/consult/orderless/embark)" section of `configuration.org`
holds an action table.

- **vertico** UI, **orderless** matching (`orderless-literal`,
  `orderless-regexp`, `orderless-initialism`; files try `basic`,
  `partial-completion`, then `orderless`), **marginalia**, **savehist** (sorts
  recent M-x commands first, replacing amx).
- **consult**: `C-x b` consult-buffer, `C-s` **fuzzy** consult-line
  (`my/consult-line-fuzzy`), `C-c f` same or `C-u` all buffers, `M-i`
  imenu / org heading, `C-x f` / `C-S-p` consult-fd, `C-c s` / `C-S-s` ripgrep,
  `C-x C-a` recent files, `C-x r b` bookmarks, `C-c r` vertico-repeat. Inside
  `C-c p`: `s` ripgrep, `S`/`C-s` ripgrep without tests, `b`
  consult-project-buffer. xref goes through `consult-xref`.
  `consult-project-function` uses `projectile-project-root`.
- Minibuffer keys: `M-o` embark-act (the same action letters as the old ivy
  actions: `s`/`v`/`n` split below / right / ace window, plus `e`, `j`, `J`,
  `r`, `I`, `k`), `C-c C-o` embark-export, `M-r` toggle fuzzy, `M-j` yank
  word/region, `C-o`/`C-S-o` grow/shrink, `C-j`/`RET`/`DEL` directory
  navigation. Embark defaults displaced by these letters were moved:
  rename-file → `R`, the vc menu → `V`.
- **Fuzzy in ripgrep**: `consult--regexp-compiler` is set to
  `my/consult-orderless-regexp-compiler`, so orderless styles (`~word` = fuzzy)
  build the regexp sent to rg. `my/orderless-toggle-flex` (`M-r`) briefly adds
  and removes a trailing space to force async consult commands to re-run.
  Both rely on consult **internals** (`consult--regexp-compiler`,
  `consult--convert-regexp`, `orderless--highlight`, `vertico--input`), so
  re-check them after consult, orderless or vertico updates.
  `consult-async-split-style` is still the default (`perl`, hence the leading
  `#`); the user was offered `'none` but hasn't decided.
- **corfu** (auto after 3 chars / 0.4s, popupinfo for docs, `C-f` location,
  `C-SPC` separator because the OS uses `M-SPC` to switch to the Greek layout),
  **cape** `cape-file`. Corfu quits on `evil-insert-state-exit-hook`. It works
  under `-nw` via Emacs 31 tty child frames (no corfu-terminal).
- **citar** replaced ivy-bibtex (`ib`, markdown `r`). A
  `hack-local-variables-hook` copies dir-local `bibtex-completion-bibliography`
  into `citar-bibliography`.

## Environment / PATH

- macOS uses `exec-path-from-shell` with its **default `-l -i`**. Don't drop
  `-i`: the Nix installer adds `~/.nix-profile/bin` from `/etc/zshrc`, which
  zsh reads only for interactive shells. Without `-i`, Dock-launched Emacs
  can't find rg, fd, etc. Interactive zsh startup costs only about 0.1s.
- On non-mac systems, a manual loop prepends `/usr/local/bin`, `~/bin`,
  `~/.local/bin`, `~/.ghcup/bin`.
- The user's Nix / home-manager config is in `~/personal/code/nixos-config` (zsh
  config generated by home-manager, e.g. `dotfiles/zshrc.nix`). That repo has
  uncommitted changes; don't edit it unless asked.

## Testing changes

There are no tests. Verify by actually starting Emacs headlessly in a pty and
dumping state to a file, then killing it:

```sh
S=<scratch dir>
TERM=xterm-256color perl -e 'alarm 90; exec @ARGV' script -q /dev/null \
  emacs -nw --debug-init --eval "(run-at-time 8 nil (lambda ()
    (with-temp-file \"$S/out.txt\"
      (insert (with-current-buffer \"*Messages*\" (buffer-string))))
    (kill-emacs 0)))" </dev/null >/dev/null 2>&1
cat $S/out.txt
```

- macOS has no `timeout`, so the `perl alarm` is the watchdog.
- To mimic a Dock launch, wrap the command in `env -i HOME=$HOME USER=$USER SHELL=/bin/zsh
  PATH=/usr/bin:/bin:/usr/sbin:/sbin` and run
  `/Applications/Emacs.app/Contents/MacOS/Emacs` directly. Leaving `SHELL` unset
  hides PATH problems, because exec-path-from-shell then falls back to `/bin/sh`.
- Drive the minibuffer with `execute-kbd-macro` (e.g. `(kbd "C-x b foo RET")`),
  or with chained `run-at-time` timers that insert text into
  `(active-minibuffer-window)` for async commands like consult-ripgrep.
  Anything that relies on `post-command-hook` (e.g. corfu popups) must be
  triggered through a key macro; calling the command from a timer won't do it.
- For lexical-binding problems, tangle with `org-babel-tangle-file` and
  `byte-compile-file` the result. Packages get loaded during compilation, so
  "free variable" warnings can miss let-bindings of not-yet-loaded vars; check
  those by hand.
- Startup takes about 2s, or about 4s when it has to re-tangle.

## Known issues and possible future work

Left alone deliberately when the user asked (don't do these unprompted):

- Tangling to a byte-compiled `.el` instead of `org-babel-load-file` on
  every start (org is loaded eagerly at startup, and also by `org-protocol`).
- `my/context-tab-line` (python tab-line) calls `project-current` and
  `which-function` on every redisplay, which may be slow over TRAMP. It is also
  added twice for python (in the python hook and in `python-base-mode-hook`).
  Harmless, but it's a duplicate.
- Pruning the ~100 `:ensure t` packages. Many languages are probably unused:
  purescript, idris, agda, dhall, scala/metals, clojure/cider, ocaml, coq,
  flow-js2, rjsx, latex-preview-pane, etc.
- The default `my/avail-themes` / `my/fonts` in `configuration.org` are always
  overridden by host files. Host files duplicate each other and the `.env`
  auto-mode patterns.
- `custom-file` is set to `custom.el` but never loaded, so Customize changes are
  discarded.
- `is-gui` / `is-term` are computed once at startup, which is wrong for
  `emacs --daemon` GUI frames.

Pending / suggested:

- **projectile → project.el**: the user is considering it, **not approved yet**.
  The plan discussed:
  - Bind `project-prefix-map` on `C-c p`, keeping the letters `f p b s S C-s c k r ! & v d D g`.
  - Set `project-switch-commands` to `project-find-file`.
  - Recreate project recent files on `C-c p e` (consult-recent-file filtered to the root).
  - Write `my/persp-switch-project` to replace persp-projectile, which is also
    used by `my/open-start-up-projects`.
  - Move the name-override advice to `project-name`, keeping
    `my/projectile-add-to-project-name-overrides` as an alias so host files
    don't change.
  - Replace the ` P[name]` mode-line function.
  - Build an Embark project map with `s/v/d/D/r`.
  - Drop `consult-project-function`.
  - Remove the `projectile-tags-command` safe-local entries.
  - Use `project-vc-extra-root-markers` for non-git roots.
  - Populate known projects once with `project-remember-projects-under ~/code`.
  - Losses: `C-c p t` test toggle, other-file, projectile test/run commands.
  - project.el doesn't cache file lists (`git ls-files` each time, about 0.2s on
    the 33k-file `~/code/monorepo`), which is the same work as the current
    projectile setup (alien indexing, caching off).
- The old packages (ivy, counsel, swiper, counsel-projectile, amx, flx,
  ivy-bibtex, company, company-quickhelp, undo-tree, racer, enh-ruby-mode) are
  still installed in `elpa/` but no longer referenced. Remove them with
  `package-delete`.
- Find-file no longer hides `.flycheck_*` / `__pycache__` (the old
  `counsel-find-file-ignore-regexp`). Dired still omits them via `dired-omit-files`.
- Ruby uses `ruby-ts-mode`. Its grammar auto-installs on the first Ruby file,
  which needs network access and a C compiler; Python's grammar auto-installs
  the same way.
- Rust uses `lsp-deferred`, which needs `rust-analyzer` on PATH.
- `lisp/projectile-tab-bar.el` isn't loaded anywhere and is a candidate for
  deletion.
- The wmctrl / xmonad `raise-frame` advice only matters on Linux (seabeast).
- Leader aliases `fd`/`sd` (pwd) and `SPC`/`t8` (fill-column indicator) are
  duplicates, kept on purpose.
- Consider `vundo` if an undo-tree style visualiser is wanted again.
