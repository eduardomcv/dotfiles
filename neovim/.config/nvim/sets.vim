" Finding files - Search down into subfolders
set path+=**

" Ignore files
set wildignore+=**/.git/*
set wildignore+=**/.vscode/*
set wildignore+=**/node_modules/*
set wildignore+=*.o
set wildignore+=*.DS_Store

" Nice menu for :find
set wildmode=longest,list,full

" Syntax and theme
set background=dark
set termguicolors
syntax enable
colorscheme gruvbox
let g:gruvbox_italic=1
let g:airline_theme='gruvbox'

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

" Vertical line at 120
set colorcolumn=120

" More space for displaying commands
set cmdheight=2

" don't show mode on last line
set noshowmode

" Line numbers
set ruler
set cursorline
set number relativenumber

" Indentation
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set smartindent

" setup undo file
set undodir=~/.vim/undodir
set undofile

" Set the terminal title at will
set title

" Automatically update files when they've been changed externally
set autoread

" Don't use swap or backups
set noswapfile
set nobackup
set nowritebackup

" Hide buffers
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

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Always show the signcolumn
set signcolumn=number

" old regexp engine incurs performance issues
set re=0

" Add asterisks in block comments
set formatoptions+=r
