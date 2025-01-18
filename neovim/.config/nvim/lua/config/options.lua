-- Leader keys must be set before calling Lazy
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Set line numbers
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"

-- Set tab size to 2 spaces
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2

-- Use system clipboard
vim.opt.clipboard:append({ "unnamedplus" })

-- Don't show mode message (the status line plugin takes care of that)
vim.opt.showmode = false
