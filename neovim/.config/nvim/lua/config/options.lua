local opt = vim.opt

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

-- Support concealed text
opt.conceallevel = 1

-- Rounded borders on floating windows
opt.winborder = "rounded"
