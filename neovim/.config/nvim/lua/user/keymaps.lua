local utils = require('user.utils')

local set = vim.keymap.set

-- Set leader to spacebar
vim.g.mapleader = ' '

-- Delete without yank
utils.nmap('x', [["_x]])
set({ 'n', 'v' }, '<leader>d', [["_d]])

-- Paste without yank
set('x', '<leader>p', [["_dP]])

-- Keep cursor in place when joining lines
utils.nmap('J', 'mzJ`z')

-- Keep cursor in the middle when navigating
utils.nmap('<C-d>', '<C-d>zz')
utils.nmap('<C-u>', '<C-u>zz')

-- Keep cursor in the middle when repeating searches
utils.nmap('n', 'nzzzv')
utils.nmap('N', 'Nzzzv')

-- Disable Q
utils.nmap('Q', '<nop>')

-- Increment/decrement
utils.nmap('+', '<C-a>')
utils.nmap('-', '<C-x>')

-- Resize window
utils.nmap('<C-w><left>', '<C-w><')
utils.nmap('<C-w><right>', '<C-w>>')
utils.nmap('<C-w><up>', '<C-w>+')
utils.nmap('<C-w><down>', '<C-w>-')

-- Tab management
utils.nmap('<leader>tn', ':tabnew<cr>')
utils.nmap('<leader>to', ':tabonly<cr>')
utils.nmap('<leader>tc', ':tabclose<cr>')
utils.nmap('<leader>tm', ':tabmove<cr>')
utils.nmap('gj', ':tabnext<cr>')
utils.nmap('gk', ':tabprev<cr>')

-- alt-j and alt-k to move lines down and up
utils.nmap('<a-j>', ':m+1<cr>')
utils.nmap('<a-k>', ':m-2<cr>')
utils.vmap('<a-j>', ":m '>+1<cr>gv=gv")
utils.vmap('<a-k>', ":m '<-2<cr>gv=gv")
