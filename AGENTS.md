# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## What this is

Personal dotfiles managed with GNU Stow. Each top-level directory (except `scripts/`, `config/`, `legacy/`) is a
stow package whose contents mirror `$HOME`: `neovim/.config/nvim/...` → `~/.config/nvim/...`. Files are symlinked,
so editing a file here edits the live config.

## Commands

- Full install (packages + stow + git identity prompt): `./scripts/install.sh` — run inside the repo, as non-root.
- Stow one package manually: `stow -t ~ <package>` (dry run: `stow -nt ~ <package>`; remove: `stow -Dt ~ <package>`).
- Hide local edits to machine-specific files from git: `scripts/git_skip_worktree.sh` (undo: `... unskip`).
  Files covered are listed in `FILES_TO_SKIP` there (zshrc/zshenv, git config, mise config, claude/opencode
  agent files). Changes to those files won't show in `git status` until unskipped.
- Logitech config (system-wide, sudo): `scripts/install_logiops_config.sh` copies `config/logid.cfg` to `/etc/logid.cfg`.
- No test suite or build.

## Layout

- A new stow package must be added to the `install_dotfiles` list in `scripts/install.sh` to be installed.
- `legacy/` holds retired configs (emacs, ghostty, hyprland/omarchy, tmux, wezterm, zed). Not installed; don't
  update them unless asked.
- `agents/.agents/AGENTS.md` is the shared global agent instruction file; `claude/.claude/CLAUDE.md` imports it via
  `@~/.agents/AGENTS.md`. opencode has its own `opencode/.config/opencode/AGENTS.md`.
  Note `claude/.claude/CLAUDE.md` is the user-global file stowed to `~/.claude/`, not a guide for this repo.

## Neovim config

- Plugins use built-in `vim.pack` (lockfile `nvim-pack-lock.json`) wrapped by the author's own `lack.nvim`
  (`lua/core/lack.lua`), which exposes a global `use({...})` taking lazy-style specs (`dependencies`, etc.).
  Each `lua/plugins/<area>.lua` calls `use(...)` then configures the plugin directly.
- New plugin files must be `require`d in `lua/plugins/init.lua`; order there matters.
- LSP uses the `vim.lsp.config(...)` API (Neovim 0.11+) with mason-lspconfig handling installation.

## Conventions

- Shell scripts: bash, `set -euo pipefail` in entrypoints, tab indentation, repo root resolved via
  `git rev-parse --show-toplevel`.
