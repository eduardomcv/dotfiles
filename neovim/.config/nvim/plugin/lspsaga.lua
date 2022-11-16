local status, lspsaga = pcall(require, "lspsaga")
if (not status) then return end

lspsaga.init_lsp_saga()

local opts = { noremap = true, silent = true }

vim.keymap.set('n', '<C-j>', '<Cmd>Lspsaga diagnostic_jump_next<CR>', opts)
vim.keymap.set('n', '<C-k>', '<Cmd>Lspsaga diagnostic_jump_prev<CR>', opts)
-- vim.keymap.set('n', 'K', '<Cmd>Lspsaga hover_doc<CR>', opts)
vim.keymap.set('n', 'gd', '<Cmd>Lspsaga lsp_finder<CR>', opts)
vim.keymap.set('n', 'gp', '<Cmd>Lspsaga peek_definition<CR>', opts)
vim.keymap.set('n', '<leader>rn', '<Cmd>Lspsaga rename<CR>', opts)
vim.keymap.set('n', '<leader>ca', '<Cmd>Lspsaga code_action<CR>', opts)
