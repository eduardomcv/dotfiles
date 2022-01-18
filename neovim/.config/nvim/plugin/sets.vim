" Nice menu for :find
set wildmode=longest,list,full

" Syntax and theme
colorscheme gruvbox
let g:gruvbox_italic=1
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

" Vertical line at 120
set colorcolumn=120

" More space for displaying commands
set cmdheight=2

" Don't show mode on last line
set noshowmode

" Line numbers
set cursorline
set number relativenumber

" Indentation
set expandtab
set shiftwidth=2
set tabstop=2
set softtabstop=2
set smartindent

" Split right on new windows
set splitright

" setup undo file
set undodir=~/.vim/undodir
set undofile

" Set the terminal title at will
set title

" Don't use swap or backups
set noswapfile
set nobackup
set nowritebackup

" Mouse support
set mouse=a

" Show matching brackets
set showmatch
set matchtime=2

" No sounds on errors
set noerrorbells
set novisualbell

" Lower timeout for mapped sequences
set timeoutlen=500

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Always show the signcolumn
set signcolumn=number

" old regexp engine incurs performance issues
set re=0

" Auto insert comment leader on enter
set formatoptions+=r

" Don't redraw while executing macros
set lazyredraw
