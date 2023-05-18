local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

local plugins = {
  'neovim/nvim-lspconfig',             -- LSP configurations
  'jose-elias-alvarez/null-ls.nvim',   -- Inject LSP diagnostics
  'j-hui/fidget.nvim',                 -- UI for LSP progress
  'onsails/lspkind.nvim',              -- vscode-like pictograms
  'rafamadriz/friendly-snippets',      -- vscode-like snippets
  'williamboman/mason.nvim',           -- Manage LSPs, linters, formatters
  'williamboman/mason-lspconfig.nvim', -- Make it easier to use lspconfig with mason
  'jayp0521/mason-null-ls.nvim',       -- Make it easier to use null-ls with mason

  {
    'hrsh7th/nvim-cmp',           -- Completion
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',     -- nvim-cmp source for neovim's built-in LSP
      'hrsh7th/cmp-buffer',       -- nvim-cmp source for buffer words
      'L3MON4D3/LuaSnip',         -- Snippet engine
      'saadparwaiz1/cmp_luasnip', -- Luasnip completion source for nvim-cmp
      {
        'David-Kunz/cmp-npm',     -- nvim-cmp source for npm
        dependencies = {
          'nvim-lua/plenary.nvim'
        },
      },
    },
  },

  {
    'nvim-treesitter/nvim-treesitter', -- Treesitter
    build = ':TSUpdate'
  },
  'windwp/nvim-ts-autotag', -- Use treesitter to auto close html tags
  'windwp/nvim-autopairs',  -- Handle brackets, quotes, etc. in pairs

  {
    'nvim-telescope/telescope.nvim', -- Fuzzy finder
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim', -- Common utilities
    },
  },
  'nvim-telescope/telescope-fzy-native.nvim',     -- Compiled FZY style sorter

  'lewis6991/gitsigns.nvim',                      -- Git decorations
  'kdheepak/lazygit.nvim',                        -- lazygit
  { 'akinsho/git-conflict.nvim', version = "*" }, -- Git conflict visualizer

  {
    'rcarriga/nvim-dap-ui',   -- UI for debugging
    dependencies = {
      'mfussenegger/nvim-dap' -- Debugging
    }
  },
  {
    "mxsdev/nvim-dap-vscode-js", -- DAP support for vscode-js-debug
    dependencies = {
      "mfussenegger/nvim-dap"
    }
  },
  {
    'nvim-neotest/neotest', -- Test runner framework
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
      'antoinemadec/FixCursorHold.nvim',
      'haydenmeade/neotest-jest', -- Jest support
    }
  },

  {
    'nvim-lualine/lualine.nvim',     -- Status line
    dependencies = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
  },
  { "catppuccin/nvim",           name = "catppuccin" }, -- Catppuccin theme
  'akinsho/nvim-bufferline.lua',                        -- Buffer line for tabs
  'lukas-reineke/indent-blankline.nvim',                -- Indentation guides
  {
    'nvim-tree/nvim-tree.lua',                          -- File tree
    dependencies = {
      'nvim-tree/nvim-web-devicons',                    -- optional, for file icons
    },
  },
  'mbbill/undotree',          -- Undo tree
  {
    'kylechui/nvim-surround', -- Surround functionality
    version = '*',
    event = 'VeryLazy'
  },
  'numToStr/Comment.nvim',              -- Commenting functionality
  'gpanders/editorconfig.nvim',         -- EditorConfig support
  'uga-rosa/ccc.nvim',                  -- Color picker and highlighter
  'folke/todo-comments.nvim',           -- Highlight keywords
  'jose-elias-alvarez/typescript.nvim', -- Better Typescript support
  'folke/neodev.nvim',                  -- Support for init.lua docs and completion
  {
    'folke/trouble.nvim',               -- Pretty diagnostics
    dependencies = {
      'nvim-tree/nvim-web-devicons',
    },
  },
  'yorickpeterse/nvim-pqf',         -- Pretty quickfix list
  'christoomey/vim-tmux-navigator', -- tmux navigation

  'github/copilot.vim',             -- Copilot
}

local opts = {}

require('lazy').setup(plugins, opts)
