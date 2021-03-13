" Specify a directory for plugins
call plug#begin(stdpath('data') . '/plugged')
" File browser
Plug 'preservim/nerdtree'
" Theme
Plug 'gruvbox-community/gruvbox'
" Completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Language syntax
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
" Git integration
Plug 'tpope/vim-fugitive'
" Status bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Fuzzy find
Plug 'kien/ctrlp.vim'
" Undo tree
Plug 'mbbill/undotree'
" Icons
Plug 'ryanoasis/vim-devicons'
" Syntax for NERDTree
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
" EditorConfig support
Plug 'editorconfig/editorconfig-vim'
" Initialize plugin system
call plug#end()

" Syntax and theme
set background=dark
let g:gruvbox_italic=1
let g:airline_theme='gruvbox'
colorscheme gruvbox
syntax enable

if (has("termguicolors"))
  set termguicolors
endif

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

" Set bash as default shell
set shell=/bin/bash

" Set the terminal title at will
set title

" Automatically update files when they've been changed externally
set autoread

" Don't use swap
set noswapfile

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

" File types
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescriptreact

" Commit messages should always wrap at 72 chars
autocmd Filetype gitcommit setlocal spell textwidth=72

" Ignore files
set wildignore+=*/.git,*/.vscode,*/node_modules,*.o,*.DS_Store

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

" Make EditorConfig ignore vim-fugitive and remote files
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" Invoke CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" CtrlP is fast enough, don't need cache
let g:ctrlp_use_caching = 0

" Use version control search for CtrlP when appropriate
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard']

" Enable NERDTree line numbers
let NERDTreeShowLineNumbers=1

" Use relative line numbers in NERDTree
autocmd FileType nerdtree setlocal relativenumber

" Start NERDTree when Vim is started without file arguments.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | NERDTree | endif

" Show hidden files by default in NERDTree
let NERDTreeShowHidden=1

" Make NERDTree respect wildignore
let NERDTreeRespectWildIgnore=1

" Set leader to spacebar
let mapleader = " "
let g:mapleader = " "

"""Key mappings
" Make splitting vertical by default for new files
noremap <c-w>n <esc>:vnew<cr>

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

"Buffer cicle (Tab and Shift+Tab)
nnoremap <s-tab> :bprevious<cr>
nnoremap <tab> :bnext<cr>

" Toggle NERDTree
nnoremap <c-n> :NERDTreeToggle<cr>
nnoremap <leader>n :NERDTreeFocus<cr>

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

" Open terminal split
nnoremap <leader>ç :below new +resize18<cr>:terminal<cr>

" Better mappings for terminal
tnoremap <esc> <c-\><c-n>
tnoremap <m-[> <esc>
tnoremap <c-v><esc> <esc>

""" CoC recommended
" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

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
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

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
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

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

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>ca  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>ce  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>cc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>co  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>cs  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>cj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>ck  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>cp  :<C-u>CocListResume<CR>

