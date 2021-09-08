""""" Plugins

call plug#begin(stdpath('data') . '/plugged')
" Theme
Plug 'arcticicestudio/nord-vim'
" LSP and autocompletion
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Language syntax
Plug 'jparise/vim-graphql'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
" Git integration
Plug 'tpope/vim-fugitive'
Plug 'APZelos/blamer.nvim'
" File browser
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
" Status bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Fuzzy find
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'
" Undo tree
Plug 'mbbill/undotree'
" EditorConfig support
Plug 'editorconfig/editorconfig-vim'
" Highlight keywords
Plug 'folke/todo-comments.nvim'
" QoL
Plug 'tpope/vim-surround'
Plug 'qpkorr/vim-bufkill'
" Initialize plugin system
call plug#end()



""""" Sets

" Path
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
colorscheme nord
let g:nord_cursor_line_number_background = 1
syntax enable
set background=dark
set termguicolors
highlight normal guibg=NONE

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

" Line break on excessively long lines
set linebreak
set textwidth=500

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



""""" Configs

" Set leader to spacebar
let mapleader = " "
let g:mapleader = " "


" Make EditorConfig ignore vim-fugitive and remote files
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" Telescope
lua << EOF
require('telescope').setup {
    extensions = {
        fzy_native = {
            override_generic_sorter = false,
            override_file_sorter = true,
        }
    }
}
require('telescope').load_extension('fzy_native')
EOF

" Todo comments
lua << EOF
local match_pattern = [[.*<(KEYWORDS)\s*]]
require("todo-comments").setup {
    -- keywords recognized as todo comments
    keywords = {
        DELETEME = {
            icon = "",
            color = "#ff0000",
        },
        FIXME = {
            icon = "",
            color = "#fff400",
        },
        HACK = {
            icon = "",
            color = "#00f9ff",
        },
        TODO = {
            icon = "",
            color = "#ed00ff",
        },
        WIP = {
            icon = "",
            color = "#ff7C00",
        }
    },
    merge_keywords = false,
    highlight = {
        before = "",
        keyword = "wide",
        after = "fg",
        pattern = match_pattern,
    },
    search = {
        pattern = match_pattern,
    }
}
EOF

" Enable git blame
let g:blamer_enabled = 1

" NERDTree
let NERDTreeShowLineNumbers=1
let NERDTreeMinimalUI=1
let NERDTreeShowHidden=1



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

" make sure relative line numbers are used in NERDTree
autocmd FileType nerdtree setlocal relativenumber
" start NERDTree when vim starts with a directory argument.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in') |
    \ execute 'NERDTree' argv()[0] | wincmd p | enew | execute 'cd '.argv()[0] | endif
" Exit Vim if NERDTree is the only window remaining in the only tab.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" If another buffer tries to replace NERDTree, put it in the other window, and bring back NERDTree.
autocmd BufEnter * if bufname('#') =~ 'NERD_tree_\d\+' && bufname('%') !~ 'NERD_tree_\d\+' && winnr('$') > 1 |
    \ let buf=bufnr() | buffer# | execute "normal! \<C-W>w" | execute 'buffer'.buf | endif

" commit messages should always wrap at 72 chars
autocmd Filetype gitcommit setlocal spell textwidth=72



""""" Keymaps

" Make splitting vertical by default for new files
noremap <c-w>n <esc>:vnew<cr>

"Copy to clipboard
nnoremap  <leader>y  "+y
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
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
nnoremap <leader>tl :tabnext<cr>
nnoremap <leader>th :tabprev<cr>

" :W and :Q to do the same as :w and :q
command! W  write
command! Q quit

" Faster saving and quitting
nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>

"Buffer cicle (Tab and Shift+Tab in normal mode)
nnoremap <s-tab> :bprevious<cr>
nnoremap <tab> :bnext<cr>

" alt-j and alt-k to move lines down and up in normal mode
nnoremap <a-j> :m+1<cr>
nnoremap <a-k> :m-2<cr>

" shift-j and shift-k to move selection down and up in visual mode
vnoremap J :m '>+1<cr>gv=gv
vnoremap K :m '<-2<cr>gv=gv

" search and replace
nnoremap <leader>s :%s//g<left><left>

" Telescope
nnoremap <C-p> :lua require('telescope.builtin').git_files()<cr>
nnoremap <leader>pf :lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>pg :lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>pb :lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>pt :lua require('telescope.builtin').help_tags()<cr>

" Fugitive
nnoremap <leader>gs :Git<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gp :Git push<cr>
nnoremap <leader>gh :diffget //2<cr>
nnoremap <leader>gl :diffget //3<cr>

" Toggle git blame
nnoremap <leader>gb :BlamerToggle<cr>

" NERD tree
nnoremap <leader>n :NERDTreeToggle<cr>

" Toggle undotree
nnoremap <leader>u :UndotreeToggle<cr>



""""" CoC setup

" Use tab for trigger completion with characters ahead and navigate.
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

nnoremap <silent> K :call <SID>show_documentation()<CR>

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups.
nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <leader>ca  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <leader>ce  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <leader>cc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <leader>co  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <leader>cs  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <leader>cj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <leader>ck  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <leader>cp  :<C-u>CocListResume<CR>
