set nocompatible
syntax on

set nowrap
set encoding=utf-8

set shell=/bin/bash

set history=1000
set undolevels=1000

set title

set autoread

let mapleader = " "
let g:mapleader = " "

set backspace=indent,eol,start

set so=10

set ruler
set cursorline
set number relativenumber

set cmdheight=2

set hid

if has('mouse')
	set mouse=a
endif

set ignorecase
set smartcase

set nohlsearch

set showmatch
set mat=2

set noerrorbells
set novisualbell

set t_vb=
set tm=500

set background=dark

if (has("termguicolors"))
  set termguicolors
endif

set t_ut=

set nobackup
set nowb
set noswapfile

set expandtab
set smarttab
set shiftwidth=2
set tabstop=2

set lbr
set tw=500

" jk to throw you into normal mode from insert mode
inoremap jk <esc>

" Tab management
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
map <leader>t<leader> :tabnext

command! W  write

nnoremap <S-Tab> :bprevious<CR>
nnoremap <Tab> :bnext<CR>

set laststatus=2

" remove whitespace
nnoremap <Leader>rws :%s/\s\+$//e<CR>

" Commit messages should always wrap at 72 chars
autocmd Filetype gitcommit setlocal spell textwidth=72

