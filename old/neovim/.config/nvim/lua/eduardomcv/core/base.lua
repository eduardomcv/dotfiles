local set = vim.opt

vim.cmd("autocmd!")

-- Ignore files
set.wildignore:append({
  "**/node_modules/*",
  "**/.git/*",
  "**/.vscode/*",
  "*.o",
  "*.DS_Store",
})

-- Encoding
set.encoding = "utf-8"
vim.scriptencoding = "utf-8"
set.fileencoding = "utf-8"

-- Line numbers
set.number = true
set.relativenumber = true

-- Indentation
set.shiftwidth = 2
set.tabstop = 2
set.softtabstop = 2
set.expandtab = true
set.smartindent = true
set.breakindent = true
set.smarttab = true

-- Highlights
set.cursorline = true
set.termguicolors = true
set.winblend = 0
set.wildoptions = "pum"
set.pumblend = 5
set.background = "dark"

-- Don't keep searched terms highlighted
set.hlsearch = false

-- Ignore case when searching
set.ignorecase = true
set.smartcase = true

-- Min 10 Lines to the cursor
set.scrolloff = 10

-- Vertical line at 120 chars
set.colorcolumn = "120"

-- Don't break in the middle of a word
set.linebreak = true

-- Nice menu for :find
set.wildmode = { "longest", "list", "full" }

-- Don't show mode on last line
set.showmode = false

-- Always show sign column
set.signcolumn = "yes"

-- Set the terminal title at will
set.title = true

-- Split right on new windows
set.splitright = true

-- Setup undo file
set.undofile = true

-- Don't use swap or backups
set.swapfile = false
set.backup = false
set.writebackup = false

-- Mouse support
set.mouse = "a"

-- Set shell to zsh
set.shell = "zsh"

set.inccommand = "split"
set.backspace = { "start", "eol", "indent" }

-- Finding files - Search down into subfolders
set.path:append({ "**" })

-- Auto insert comment leader on enter
set.formatoptions:append({ "r" })

-- Undercurl
vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])

-- Turn off paste mode when leaving insert
vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = "*",
  command = "set nopaste",
})
