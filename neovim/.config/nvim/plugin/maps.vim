" Set leader to spacebar
let mapleader = " "

" Vertical split on new buffer
noremap <c-w>n <esc>:vnew<cr>

" Copy to clipboard
nnoremap <leader>cy "+y
vnoremap <leader>cy "+y
nnoremap <leader>cY "+yg_
nnoremap <leader>cyy "+yy

" Paste from clipboard
nnoremap <leader>cp "+p
nnoremap <leader>cP "+P
vnoremap <leader>cp "+p
vnoremap <leader>cP "+P

" Delete without yank
nnoremap <leader>d "_d
nnoremap x "_x

" Select all
nmap <C-a> gg<S-v>G

" Faster window change
nnoremap <leader>h :wincmd h<cr>
nnoremap <leader>j :wincmd j<cr>
nnoremap <leader>k :wincmd k<cr>
nnoremap <leader>l :wincmd l<cr>

" Resize window
nmap <C-w><left> <C-w><
nmap <C-w><right> <C-w>>
nmap <C-w><up> <C-w>+
nmap <C-w><down> <C-w>-

" Tab management
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove<cr>
nnoremap <leader>th :tabprevious<cr>
nnoremap <leader>tl :tabnext<cr>

" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>

" alt-j and alt-k to move lines down and up in normal mode
nnoremap <a-j> :m+1<cr>
nnoremap <a-k> :m-2<cr>

" shift-j and shift-k to move selection down and up in visual mode
vnoremap J :m '>+1<cr>gv=gv
vnoremap K :m '<-2<cr>gv=gv

" Search and replace
nnoremap <leader>s :%s//g<left><left>

" Increment/decrement
nnoremap + <C-a>
nnoremap - <C-x>
