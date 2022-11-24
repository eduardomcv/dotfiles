local status, lspsaga = pcall(require, "lspsaga")
if (not status) then return end

lspsaga.init_lsp_saga()

local set = vim.keymap.set
local opts = { noremap = true, silent = true }

set('n', '<C-j>', '<Cmd>Lspsaga diagnostic_jump_next<CR>', opts)
set('n', '<C-k>', '<Cmd>Lspsaga diagnostic_jump_prev<CR>', opts)
set('n', 'K', '<Cmd>Lspsaga hover_doc<CR>', opts)
set('n', 'gd', '<Cmd>Lspsaga lsp_finder<CR>', opts)
set('n', 'gp', '<Cmd>Lspsaga peek_definition<CR>', opts)
set('n', '<leader>rn', '<Cmd>Lspsaga rename<CR>', opts)
set('n', '<leader>ca', '<Cmd>Lspsaga code_action<CR>', opts)
