local status, packer = pcall(require, "packer")
if (not status) then
  print("Packer is not installed")
  return
end

vim.cmd [[packadd packer.nvim]]

packer.startup(function(use)
  use 'wbthomason/packer.nvim' -- Plugin manager

  use 'neovim/nvim-lspconfig' -- LSP
  use 'hrsh7th/cmp-buffer' -- nvim-cmp source for buffer words
  use 'hrsh7th/cmp-nvim-lsp' -- nvim-cmp source for neovim's built-in LSP
  use 'hrsh7th/nvim-cmp' -- Completion
  use 'onsails/lspkind-nvim' -- vscode-like pictograms
  use 'L3MON4D3/LuaSnip' -- Snippet engine
  use 'saadparwaiz1/cmp_luasnip' -- Luasnip completion source for nvim-cmp
  use 'glepnir/lspsaga.nvim' -- LSP UI
  use 'jose-elias-alvarez/null-ls.nvim' -- Inject LSP diagnostics
  use 'williamboman/mason.nvim' -- Manage LSPs, linters, formatters
  use 'williamboman/mason-lspconfig.nvim' -- Make it easier to use lspconfig with mason
  use 'jayp0521/mason-null-ls.nvim' -- Make it easier to use null-ls with mason

  use {
    'nvim-treesitter/nvim-treesitter', -- Treesitter
    run = ':TSUpdate'
  }
  use 'windwp/nvim-autopairs' -- Handle brackets, quotes, etc. in pairs
  use 'windwp/nvim-ts-autotag' -- Use treesitter to auto close html tags

  use 'nvim-lua/plenary.nvim' -- Common utilities
  use 'nvim-telescope/telescope.nvim' -- Fuzzy finder
  use 'nvim-telescope/telescope-fzy-native.nvim' -- Compiled FZY style sorter
  use 'nvim-telescope/telescope-file-browser.nvim' -- File browser

  use 'lewis6991/gitsigns.nvim' -- Git decorations
  use 'dinhhuy258/git.nvim' -- Git blamer, browser, commands, etc in nvim

  use 'nvim-lualine/lualine.nvim' -- Status line
  use 'Mofiqul/vscode.nvim' -- VSCode theme
  use 'nvim-tree/nvim-web-devicons' -- File icons
  use 'akinsho/nvim-bufferline.lua' -- Buffer line for tabs
  use 'norcalli/nvim-colorizer.lua' -- Color highlighter
  use 'lukas-reineke/indent-blankline.nvim' -- Indentation guides
  use 'folke/todo-comments.nvim' -- Highlight keywords
  use 'kylechui/nvim-surround' -- Surround functionality
  use 'numToStr/Comment.nvim' -- Commenting functionality
  use 'mbbill/undotree' -- Undo tree
  use 'gpanders/editorconfig.nvim' -- EditorConfig support

  use 'David-Kunz/jester' -- Run jest tests easily
  use 'mfussenegger/nvim-dap' -- Debugging
end)
