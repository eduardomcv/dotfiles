local o = vim.opt

-- Set line numbers
o.number = true
o.relativenumber = true
o.signcolumn = "yes"

-- Set tab size to 2 spaces
o.tabstop = 2
o.shiftwidth = 2

-- Use system clipboard
o.clipboard:append({ "unnamedplus" })

-- Don't show mode message (the status line plugin takes care of that)
o.showmode = false

-- Highlight cursor line
o.cursorline = true

-- Cursor scroll offset
o.scrolloff = 8
o.sidescrolloff = 8

-- Persistent undo
o.undofile = true
o.undolevels = 10000

-- Support concealed text
o.conceallevel = 1

-- Rounded borders on floating windows
o.winborder = "rounded"

-- Don't use swapfile
o.swapfile = false

-- Enable 24-bit RGB color
o.termguicolors = true
