local set = vim.keymap.set

-- Set leader to spacebar
vim.g.mapleader = ' '

-- Delete without yank
set('n', 'x', '"_x')
set('v', '<leader>d', '"_d')

-- Increment/decrement
set('n', '+', '<C-a>')
set('n', '-', '<C-x>')

-- Select all
set('n', '<C-a>', 'gg<S-v>G')

-- Move window
set('n', '<leader>h', '<C-w>h')
set('n', '<leader>k', '<C-w>k')
set('n', '<leader>j', '<C-w>j')
set('n', '<leader>l', '<C-w>l')

-- Resize window
set('n', '<C-w><left>', '<C-w><')
set('n', '<C-w><right>', '<C-w>>')
set('n', '<C-w><up>', '<C-w>+')
set('n', '<C-w><down>', '<C-w>-')

-- " Tab management
set('n', '<leader>tn', ':tabnew<cr>')
set('n', '<leader>to', ':tabonly<cr>')
set('n', '<leader>tc', ':tabclose<cr>')
set('n', '<leader>tm', ':tabmove<cr>')
set('n', 'gj', ':tabnext<cr>')
set('n', 'gk', ':tabprev<cr>')

-- alt-j and alt-k to move lines down and up
set('n', '<a-j>', ':m+1<cr>')
set('n', '<a-k>', ':m-2<cr>')
set('v', '<a-j>', ":m '>+1<cr>gv=gv")
set('v', '<a-k>', ":m '<-2<cr>gv=gv")
