" init autocmd
autocmd!

runtime ./plug.vim
runtime ./sets.vim
runtime ./maps.vim

" Enable git blame
let g:blamer_enabled = 1



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

" commit messages should always wrap at 72 chars
autocmd Filetype gitcommit setlocal spell textwidth=72

" sometimes syntax highlighting can get out of sync
autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart
autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear


