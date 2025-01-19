local opt = vim.opt

-- Leader keys must be set before calling Lazy
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Set line numbers
opt.number = true
opt.relativenumber = true
opt.signcolumn = "yes"

-- Set tab size to 2 spaces
opt.tabstop = 2
opt.shiftwidth = 2

-- Use system clipboard
opt.clipboard:append({ "unnamedplus" })

-- Don't show mode message (the status line plugin takes care of that)
opt.showmode = false

-- Highlight cursor line
opt.cursorline = true

-- Cursor scroll offset
opt.scrolloff = 8
opt.sidescrolloff = 8

-- Persistent undo
opt.undofile = true
opt.undolevels = 10000
