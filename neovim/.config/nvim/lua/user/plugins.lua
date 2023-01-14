-- ensure packer is installed
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

local ok, packer = pcall(require, 'packer')
if not ok then
  print('Packer is not installed')
  return
end

vim.cmd [[packadd packer.nvim]]

return packer.startup(function(use)
  use 'wbthomason/packer.nvim' -- Plugin manager

  use 'neovim/nvim-lspconfig' -- LSP configurations
  use 'jose-elias-alvarez/null-ls.nvim' -- Inject LSP diagnostics
  use {
    'glepnir/lspsaga.nvim', -- LSP UI
    branch = 'main'
  }
  use 'j-hui/fidget.nvim' -- UI for LSP progress
  use 'onsails/lspkind.nvim' -- vscode-like pictograms
  use 'rafamadriz/friendly-snippets' -- vscode-like snippets

  use 'williamboman/mason.nvim' -- Manage LSPs, linters, formatters
  use 'williamboman/mason-lspconfig.nvim' -- Make it easier to use lspconfig with mason
  use 'jayp0521/mason-null-ls.nvim' -- Make it easier to use null-ls with mason

  use {
    'hrsh7th/nvim-cmp', -- Completion
    requires = {
      'hrsh7th/cmp-nvim-lsp', -- nvim-cmp source for neovim's built-in LSP
      'hrsh7th/cmp-buffer', -- nvim-cmp source for buffer words
      'L3MON4D3/LuaSnip', -- Snippet engine
      'saadparwaiz1/cmp_luasnip', -- Luasnip completion source for nvim-cmp
      {
        'David-Kunz/cmp-npm', -- nvim-cmp source for npm
        requires = {
          'nvim-lua/plenary.nvim'
        }
      }
    }
  }

  use {
    'nvim-treesitter/nvim-treesitter', -- Treesitter
    run = ':TSUpdate'
  }
  use 'windwp/nvim-ts-autotag' -- Use treesitter to auto close html tags
  use 'windwp/nvim-autopairs' -- Handle brackets, quotes, etc. in pairs

  use {
    'nvim-telescope/telescope.nvim', -- Fuzzy finder
    branch = '0.1.x',
    requires = {
      'nvim-lua/plenary.nvim' -- Common utilities
    }
  }
  use 'nvim-telescope/telescope-fzy-native.nvim' -- Compiled FZY style sorter

  use 'lewis6991/gitsigns.nvim' -- Git decorations
  use 'kdheepak/lazygit.nvim' -- lazygit

  use {
    'nvim-lualine/lualine.nvim', -- Status line
    requires = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    }
  }
  use 'Mofiqul/vscode.nvim' -- VSCode theme
  use 'akinsho/nvim-bufferline.lua' -- Buffer line for tabs
  use 'lukas-reineke/indent-blankline.nvim' -- Indentation guides

  use {
    'rcarriga/nvim-dap-ui', -- UI for debugger
    requires = {
      'mfussenegger/nvim-dap' -- Debugging
    }
  }
  use {
    "mxsdev/nvim-dap-vscode-js", -- DAP support for vscode-js-debug
    requires = {
      "mfussenegger/nvim-dap"
    }
  }
  use {
    'nvim-neotest/neotest', -- Test runner framework
    requires = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
      'antoinemadec/FixCursorHold.nvim',
      'haydenmeade/neotest-jest', -- Jest support
    }
  }

  use {
    'nvim-tree/nvim-tree.lua', -- File tree
    requires = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
  }
  use 'mbbill/undotree' -- Undo tree
  use 'tpope/vim-surround' -- Surround functionality
  use 'numToStr/Comment.nvim' -- Commenting functionality
  use 'gpanders/editorconfig.nvim' -- EditorConfig support
  use 'uga-rosa/ccc.nvim' -- Color picker and highlighter
  use 'folke/todo-comments.nvim' -- Highlight keywords
  use 'jose-elias-alvarez/typescript.nvim' -- Better Typescript support
  use 'folke/neodev.nvim' -- Support for init.lua docs and completion
  use {
    'folke/trouble.nvim', -- Pretty diagnostics
    requires = 'nvim-tree/nvim-web-devicons',
  }
  use 'vimwiki/vimwiki' -- Vimwiki

  if packer_bootstrap then
    packer.sync()
  end
end)
