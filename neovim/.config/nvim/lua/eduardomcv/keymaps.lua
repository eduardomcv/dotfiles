local set = vim.keymap.set

local function nmap(lhr, rhs)
  set('n', lhr, rhs)
end

local function vmap(lhr, rhs)
  set('v', lhr, rhs)
end

-- Set leader to spacebar
vim.g.mapleader = ' '

-- Delete without yank
nmap('x', [["_x]])
set({ 'n', 'v' }, '<leader>d', [["_d]])

-- Paste without yank
set('x', '<leader>p', [["_dP]])

-- Keep cursor in place when joining lines
nmap('J', 'mzJ`z')

-- Keep cursor in the middle when navigating
nmap('<C-d>', '<C-d>zz')
nmap('<C-u>', '<C-u>zz')

-- Keep cursor in the middle when repeating searches
nmap('n', 'nzzzv')
nmap('N', 'Nzzzv')

nmap('Q', '<nop>')

-- Increment/decrement
nmap('+', '<C-a>')
nmap('-', '<C-x>')

-- Select all
nmap('<C-a>', 'gg<S-v>G')

-- Move window
nmap('<leader>h', '<C-w>h')
nmap('<leader>k', '<C-w>k')
nmap('<leader>j', '<C-w>j')
nmap('<leader>l', '<C-w>l')

-- Resize window
nmap('<C-w><left>', '<C-w><')
nmap('<C-w><right>', '<C-w>>')
nmap('<C-w><up>', '<C-w>+')
nmap('<C-w><down>', '<C-w>-')

-- Tab management
nmap('<leader>tn', ':tabnew<cr>')
nmap('<leader>to', ':tabonly<cr>')
nmap('<leader>tc', ':tabclose<cr>')
nmap('<leader>tm', ':tabmove<cr>')
nmap('gj', ':tabnext<cr>')
nmap('gk', ':tabprev<cr>')

-- alt-j and alt-k to move lines down and up
nmap('<a-j>', ':m+1<cr>')
nmap('<a-k>', ':m-2<cr>')
vmap('<a-j>', ":m '>+1<cr>gv=gv")
vmap('<a-k>', ":m '<-2<cr>gv=gv")
