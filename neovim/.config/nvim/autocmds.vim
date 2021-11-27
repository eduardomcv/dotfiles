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

" Turn off paste mode when leaving insert
autocmd InsertLeave * set nopaste

" Commit messages should always wrap at 72 chars
autocmd Filetype gitcommit setlocal spell textwidth=72

" File types
autocmd BufNewFile,BufRead *.tsx setf typescriptreact
autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd BufNewFile,BufRead *.mdx set filetype=markdown
autocmd BufNewFile,BufRead *.flow set filetype=javascript
autocmd BufNewFile,BufRead *.es6 setf javascript

autocmd FileType yaml setlocal shiftwidth=2 tabstop=2
