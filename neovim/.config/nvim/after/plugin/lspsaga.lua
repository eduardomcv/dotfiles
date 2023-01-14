local ok_lspsaga, lspsaga = pcall(require, "lspsaga")
if not ok_lspsaga then return end

local ok_colors, colors = pcall(require, 'vscode.colors')
if not ok_colors then return end

local utils = require('user.utils')
local c = colors.get_colors()

lspsaga.setup({
  ui = {
    border = 'rounded',
    colors = {
      normal_bg = c.vscPopupBack,
      magenta = c.vscPink,
      cyan = c.vscLightBlue,
    }
  }
})

utils.nmap('<C-j>', '<Cmd>Lspsaga diagnostic_jump_next<CR>')
utils.nmap('<C-k>', '<Cmd>Lspsaga diagnostic_jump_prev<CR>')
utils.nmap('K', '<Cmd>Lspsaga hover_doc<CR>')
utils.nmap('gd', '<Cmd>Lspsaga lsp_finder<CR>')
utils.nmap('gp', '<Cmd>Lspsaga peek_definition<CR>')
utils.nmap('<leader>rn', '<Cmd>Lspsaga rename<CR>')
utils.nmap('<leader>ca', '<Cmd>Lspsaga code_action<CR>')
utils.nmap("<leader>o", "<cmd>Lspsaga outline<CR>")
