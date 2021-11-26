" Vertical split on new buffer
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

" Delete without yank
nnoremap <leader>d "_d

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
nnoremap <leader>tl :tabnext<cr>
nnoremap <leader>th :tabprev<cr>

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
nnoremap <leader>ph :lua require('telescope.builtin').help_tags()<cr>

" Fugitive
nnoremap <leader>gs :Git<cr>
nnoremap <leader>gh :diffget //2<cr>
nnoremap <leader>gl :diffget //3<cr>

" Toggle git blame
nnoremap <leader>gb :BlamerToggle<cr>

" NERD tree
nnoremap <leader>n :NERDTreeToggle<cr>

" Toggle undotree
nnoremap <leader>u :UndotreeToggle<cr>



