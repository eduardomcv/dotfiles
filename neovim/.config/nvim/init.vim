" Finding files - Search down into subfolders
set path+=**

" Ignore files
set wildignore+=**/.git/*
set wildignore+=**/.vscode/*
set wildignore+=**/node_modules/*
set wildignore+=*.o
set wildignore+=*.DS_Store


""" Plugins

call plug#begin(stdpath('data') . '/plugged')

""" Core
Plug 'nvim-lua/plenary.nvim'                                  " Dependencies
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}   " Tree sitter
Plug 'neovim/nvim-lspconfig'                                  " LSP

""" Completion and snippets
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-buffer'
Plug 'L3MON4D3/LuaSnip'
Plug 'onsails/lspkind-nvim'
Plug 'tami5/lspsaga.nvim'

""" Theme
Plug 'gruvbox-community/gruvbox'
Plug 'folke/lsp-colors.nvim'

""" Git
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'APZelos/blamer.nvim'

""" Fuzzy finder
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'

""" QoL
Plug 'editorconfig/editorconfig-vim'                          " EditorConfig support
Plug 'tpope/vim-surround'                                     " Surround functionality
Plug 'folke/todo-comments.nvim'                               " Highlight keywords
Plug 'lukas-reineke/indent-blankline.nvim'                    " Indentation guides
Plug 'windwp/nvim-autopairs'                                  " Handle brackets, quotes, etc. in pairs
Plug 'kyazdani42/nvim-web-devicons'                           " Icons
Plug 'nvim-lualine/lualine.nvim'                              " Status bar
Plug 'mbbill/undotree'                                        " Undo tree
Plug 'nathom/filetype.nvim'                                   " Faster startup

call plug#end()


""" Functions

" Trim whitespace before writing buffer
fun! TrimWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfun

augroup TrimWhitespaceBeforeWrite
  autocmd!
  autocmd BufWritePre * :call TrimWhitespace()
augroup END

" Commit messages should always wrap at 72 chars
autocmd Filetype gitcommit setlocal spell textwidth=72

