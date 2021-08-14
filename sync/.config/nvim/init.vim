""""" Plugins

call plug#begin(stdpath('data') . '/plugged')
" Theme
Plug 'gruvbox-community/gruvbox'
" LSP and autocompletion
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/completion-nvim'
" Language syntax
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
" Git integration
Plug 'tpope/vim-fugitive'
" Improve netrw
Plug 'tpope/vim-vinegar'
" Status bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Fuzzy find
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
" Undo tree
Plug 'mbbill/undotree'
" EditorConfig support
Plug 'editorconfig/editorconfig-vim'
" Initialize plugin system
call plug#end()



""""" Sets

" Syntax and theme
set background=dark
let g:gruvbox_italic=1
let g:airline_theme='gruvbox'
colorscheme gruvbox
syntax enable
set termguicolors
set background=dark

" Don't wrap text
set nowrap

" Don't keep searched terms highlighted
set nohlsearch

" Ignore case when searching
set ignorecase
set smartcase

" Source local configs if available
set exrc
set secure

" Block cursor
set guicursor=

" Min 10 Lines to the cursor
set scrolloff=10

" Give more space for displaying commands
set cmdheight=2

" Line numbers
set ruler
set cursorline
set relativenumber

" Indentation
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set smartindent

" Line break on excessively long lines
set linebreak
set textwidth=500

" setup undo file
set undodir=~/.vim/undodir
set undofile

" Set bash as default shell
set shell=/bin/bash

" Set the terminal title at will
set title

" Automatically update files when they've been changed externally
set autoread

" Don't use swap or backups
set noswapfile
set nobackup
set nowritebackup

set hidden

" Mouse support
if has('mouse')
	set mouse=a
endif

" Show matching brackets
set showmatch
set matchtime=2

" No sounds on errors
set noerrorbells
set novisualbell
set tm=500

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c

set updatetime=300
set signcolumn=number

" Ignore files
set wildignore+=**/*/.git
set wildignore+=**/*/.vscode
set wildignore+=**/*/node_modules
set wildignore+=*.o
set wildignore+=*.DS_Store



""""" Configs

" Make EditorConfig ignore vim-fugitive and remote files
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" netrw
let g:netrw_banner = 0
let g:netrw_browse_split = 4
let g:netrw_liststyle = 3
let g:netrw_altv = 1
let g:netrw_winsize = 20

" LSP
lua require'lspconfig'.tsserver.setup{on_attach=require'completion'.on_attach}



""""" Autocmds

" Function for trimming whitespace
fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun

" Trim whitespace before writing buffer
augroup TRIM_WHITESPACE_BEFORE_WRITING
    autocmd!
    autocmd BufWritePre * :call TrimWhitespace()
augroup END

" Commit messages should always wrap at 72 chars
autocmd Filetype gitcommit setlocal spell textwidth=72



""""" Keymaps

" Set leader to spacebar
let mapleader = " "
let g:mapleader = " "

" Make splitting vertical by default for new files
noremap <c-w>n <esc>:vnew<cr>

" Open netrw vertically
nnoremap <c-n> :Vex<cr>

"Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

"Paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P

" Faster window change
nnoremap <leader>h :wincmd h<cr>
nnoremap <leader>j :wincmd j<cr>
nnoremap <leader>k :wincmd k<cr>
nnoremap <leader>l :wincmd l<cr>

" Tab management
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove<cr>
nnoremap <leader>t<leader> :tabnext<cr>

" :W and :Q to do the same as :w and :q
command! W  write
command! Q quit

" Faster saving and quitting
nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>

"Buffer cicle (Tab and Shift+Tab in normal mode)
nnoremap <s-tab> :bprevious<cr>
nnoremap <tab> :bnext<cr>

" Toggle undotree
nnoremap <leader>u :UndotreeToggle<cr>

" alt-j and alt-k to move lines down and up in normal mode
nnoremap <a-j> :m+1<cr>
nnoremap <a-k> :m-2<cr>

" shift-j and shift-k to move selection down and up in visual mode
vnoremap J :m '>+1<cr>gv=gv
vnoremap K :m '<-2<cr>gv=gv

" Easy config sourcing
nnoremap <leader><cr> :source ~/.config/nvim/init.vim<cr>

" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Open terminal split
nnoremap <leader>ç :below new +resize18<cr>:terminal<cr>

" Better mappings for terminal
tnoremap <esc> <c-\><c-n>
tnoremap <m-[> <esc>
tnoremap <c-v><esc> <esc>

" Telescope
nnoremap <c-p> :Telescope find_files<cr>