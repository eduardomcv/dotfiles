local keymap = vim.keymap

-- Set leader to spacebar
vim.g.mapleader = ' '

-- " Copy to clipboard
keymap.set({ 'n', 'v' }, '<leader>cy', '"+y')
keymap.set('n', '<leader>cY', '"+yg_')
keymap.set('n', '<leader>cyy', '"+yy')

-- " Paste from clipboard
keymap.set({ 'n', 'v' }, '<leader>cp', '"+p')
keymap.set({ 'n', 'v' }, '<leader>cP', '"+P')

-- Delete without yank
keymap.set('n', 'x', '"_x')
keymap.set('v', '<leader>d', '"_d')

-- Increment/decrement
keymap.set('n', '+', '<C-a>')
keymap.set('n', '-', '<C-x>')

-- Select all
keymap.set('n', '<C-a>', 'gg<S-v>G')

-- New tab
keymap.set('n', 'te', ':tabedit')

-- Split window
keymap.set('n', '<leader>ss', ':split<Return><C-w>w')
keymap.set('n', '<leader>sv', ':vsplit<Return><C-w>w')

-- Move window
keymap.set('n', '<leader>h', '<C-w>h')
keymap.set('n', '<leader>k', '<C-w>k')
keymap.set('n', '<leader>j', '<C-w>j')
keymap.set('n', '<leader>l', '<C-w>l')

-- Resize window
keymap.set('n', '<C-w><left>', '<C-w><')
keymap.set('n', '<C-w><right>', '<C-w>>')
keymap.set('n', '<C-w><up>', '<C-w>+')
keymap.set('n', '<C-w><down>', '<C-w>-')

-- " Tab management
keymap.set('n', '<leader>tn', ':tabnew<cr>')
keymap.set('n', '<leader>to', ':tabonly<cr>')
keymap.set('n', '<leader>tc', ':tabclose<cr>')
keymap.set('n', '<leader>tm', ':tabmove<cr>')

-- alt-j and alt-k to move lines down and up in normal mode
keymap.set('n', '<a-j>', ':m+1<cr>')
keymap.set('n', '<a-k>', ':m-2<cr>')

-- shift-j and shift-k to move selection down and up in visual mode
keymap.set('v', 'J', ":m '>+1<cr>gv=gv")
keymap.set('v', 'K', ":m '<-2<cr>gv=gv")

-- Search and replace
keymap.set('n', '<leader-s>', ':%s//g<left><left>')
