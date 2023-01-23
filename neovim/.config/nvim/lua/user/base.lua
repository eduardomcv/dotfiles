vim.cmd("autocmd!")

-- Ignore files
vim.opt.wildignore:append {
  '**/node_modules/*',
  '**/.git/*',
  '**/.vscode/*',
  '*.o',
  '*.DS_Store',
}

-- Encoding
vim.opt.encoding = 'utf-8'
vim.scriptencoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'

-- Line numbers
vim.opt.number = true
vim.opt.relativenumber = true

-- Indentation
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.breakindent = true
vim.opt.smarttab = true

-- Highlights
vim.opt.cursorline = true
vim.opt.termguicolors = true
vim.opt.winblend = 0
vim.opt.wildoptions = 'pum'
vim.opt.pumblend = 5
vim.opt.background = 'dark'


-- Don't keep searched terms highlighted
vim.opt.hlsearch = false

-- Ignore case when searching
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Min 10 Lines to the cursor
vim.opt.scrolloff = 10

-- Vertical line at 120 chars
vim.opt.colorcolumn = '120'

-- Don't break in the middle of a word
vim.opt.linebreak = true

-- Nice menu for :find
vim.opt.wildmode = { 'longest', 'list', 'full' }

-- Don't show mode on last line
vim.opt.showmode = false

-- Always show sign column
vim.opt.signcolumn = 'yes'

-- Set the terminal title at will
vim.opt.title = true

-- Split right on new windows
vim.opt.splitright = true

-- Setup undo file
vim.opt.undofile = true

-- Don't use swap or backups
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false

-- Mouse support
vim.opt.mouse = 'a'

-- Set shell to zsh
vim.opt.shell = 'zsh'

vim.opt.inccommand = 'split'
vim.opt.backspace = { 'start', 'eol', 'indent' }

-- Finding files - Search down into subfolders
vim.opt.path:append { '**' }

-- Auto insert comment leader on enter
vim.opt.formatoptions:append { 'r' }

-- Undercurl
vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])

-- Turn off paste mode when leaving insert
vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = '*',
  command = "set nopaste"
})
