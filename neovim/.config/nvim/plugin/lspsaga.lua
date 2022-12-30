local ok, lspsaga = pcall(require, "lspsaga")
if not ok then return end

local utils = require('user.utils')

lspsaga.init_lsp_saga()

utils.nmap('<C-j>', '<Cmd>Lspsaga diagnostic_jump_next<CR>')
utils.nmap('<C-k>', '<Cmd>Lspsaga diagnostic_jump_prev<CR>')
utils.nmap('K', '<Cmd>Lspsaga hover_doc<CR>')
utils.nmap('gd', '<Cmd>Lspsaga lsp_finder<CR>')
utils.nmap('gp', '<Cmd>Lspsaga peek_definition<CR>')
utils.nmap('<leader>rn', '<Cmd>Lspsaga rename<CR>')
utils.nmap('<leader>ca', '<Cmd>Lspsaga code_action<CR>')
