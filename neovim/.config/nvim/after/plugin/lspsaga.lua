local ok_lspsaga, lspsaga = pcall(require, "lspsaga")
if not ok_lspsaga then return end

local ok_colors, colors = pcall(require, 'tokyonight.colors')
if not ok_colors then return end

local utils = require('user.utils')
local c = colors.setup({ style = 'night' })

lspsaga.setup({
  ui = {
    border = 'rounded',
    colors = {
      normal_bg = c.bg_dark,
      red = c.red,
      magenta = c.magenta,
      orange = c.orange,
      yellow = c.yellow,
      green = c.green,
      cyan = c.cyan,
      blue = c.blue,
      purple = c.purple,
      white = '#d1d4cf',
      black = '#1c1c19',
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
