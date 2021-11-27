call plug#begin(stdpath('data') . '/plugged')

" Tree sitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" LSP
Plug 'neovim/nvim-lspconfig'

" Completion and snippets
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-buffer'
Plug 'L3MON4D3/LuaSnip'
Plug 'onsails/lspkind-nvim'
Plug 'tami5/lspsaga.nvim', { 'branch': 'nvim51' }

" Theme and colors
Plug 'gruvbox-community/gruvbox'
Plug 'folke/lsp-colors.nvim'

" Git
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'APZelos/blamer.nvim'

" Status bar
Plug 'nvim-lualine/lualine.nvim'

" Fuzzy finder
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'

" EditorConfig support
Plug 'editorconfig/editorconfig-vim'

" Highlight keywords
Plug 'folke/todo-comments.nvim'

" Undotree
Plug 'mbbill/undotree'

" QoL
Plug 'tpope/vim-surround'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'windwp/nvim-autopairs'
Plug 'kyazdani42/nvim-web-devicons'

call plug#end()
