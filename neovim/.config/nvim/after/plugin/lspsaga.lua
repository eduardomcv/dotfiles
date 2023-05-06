local ok_lspsaga, lspsaga = pcall(require, "lspsaga")
if not ok_lspsaga then return end

local u = require('user.utils')

lspsaga.setup({
  ui = {
    border = 'rounded',
    kind = require("catppuccin.groups.integrations.lsp_saga").custom_kind(),
  },
  finder = {
    keys = {
      expand_or_jump = '<CR>',
    },
  },
})

u.nmap('g]', ':Lspsaga diagnostic_jump_next<CR>')
u.nmap('g[', ':Lspsaga diagnostic_jump_prev<CR>')
u.nmap('K', ':Lspsaga hover_doc<CR>')
u.nmap('gd', ':Lspsaga goto_definition<CR>')
u.nmap('gt', ':Lspsaga peek_type_definition<CR>')
u.nmap('gf', ':Lspsaga lsp_finder<CR>')
u.nmap('gp', ':Lspsaga peek_definition<CR>')
u.nmap('<leader>rn', ':Lspsaga rename<CR>')
u.nmap('<leader>ca', ':Lspsaga code_action<CR>')
u.nmap("<leader>o", ":Lspsaga outline<CR>")
