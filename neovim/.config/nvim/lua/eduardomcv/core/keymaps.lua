local u = require('eduardomcv.utils')

local set = vim.keymap.set

-- Set leader to spacebar
vim.g.mapleader = ' '

-- Delete without yank
u.nmap('x', [["_x]])
set({ 'n', 'v' }, '<leader>d', [["_d]])

-- Paste without yank
set('x', '<leader>p', [["_dP]])

-- Keep cursor in place when joining lines
u.nmap('J', 'mzJ`z')

-- Keep cursor in the middle when navigating
u.nmap('<C-d>', '<C-d>zz')
u.nmap('<C-u>', '<C-u>zz')

-- Keep cursor in the middle when repeating searches
u.nmap('n', 'nzzzv')
u.nmap('N', 'Nzzzv')

-- Disable Q
u.nmap('Q', '<nop>')

-- Increment/decrement
u.nmap('+', '<C-a>')
u.nmap('-', '<C-x>')

-- Resize window
u.nmap('<C-w><left>', '<C-w><')
u.nmap('<C-w><right>', '<C-w>>')
u.nmap('<C-w><up>', '<C-w>+')
u.nmap('<C-w><down>', '<C-w>-')

-- alt-j and alt-k to move lines down and up
u.nmap('<a-j>', ':m+1<cr>')
u.nmap('<a-k>', ':m-2<cr>')
u.vmap('<a-j>', ":m '>+1<cr>gv=gv")
u.vmap('<a-k>', ":m '<-2<cr>gv=gv")
