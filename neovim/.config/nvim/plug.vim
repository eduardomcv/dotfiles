call plug#begin(stdpath('data') . '/plugged')

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'neovim/nvim-lspconfig'
Plug 'tami5/lspsaga.nvim', { 'branch': 'nvim51' }

" Theme
Plug 'gruvbox-community/gruvbox'
Plug 'folke/lsp-colors.nvim'

" Git
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'APZelos/blamer.nvim'

" Status bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Fuzzy finder
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'

" EditorConfig support
Plug 'editorconfig/editorconfig-vim'

" Highlight keywords
Plug 'folke/todo-comments.nvim'

" QoL
Plug 'tpope/vim-surround'
Plug 'qpkorr/vim-bufkill'
Plug 'Yggdroot/indentLine'
Plug 'mbbill/undotree'

call plug#end()
