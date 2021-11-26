call plug#begin(stdpath('data') . '/plugged')

" Dependencies
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" Theme
Plug 'gruvbox-community/gruvbox'

" Language syntax
Plug 'pangloss/vim-javascript'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'jparise/vim-graphql'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }

" Git
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'APZelos/blamer.nvim'

" File browser
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

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
