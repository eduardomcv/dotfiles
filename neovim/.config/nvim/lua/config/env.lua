-- Add mise shims to PATH
vim.env.PATH = vim.fn.expand("$HOME/.local/share/mise/shims") .. ":" .. vim.env.PATH
