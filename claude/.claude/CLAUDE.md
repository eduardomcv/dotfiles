# Claude directives

## 1. Global agent directives

@~/.agents/AGENTS.md

## 2. Claude-specific directives

### File Links in the Claude Desktop App

**Use absolute paths in file links when working in a worktree.** The desktop
app resolves relative links against the original project folder, not the
session's working directory, so a worktree-relative link does not open. Write
`[name](/abs/path/to/file)`. This overrides the default of linking relative to
the working directory.

## 3. Other directives
