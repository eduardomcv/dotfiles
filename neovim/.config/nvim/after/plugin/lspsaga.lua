local ok_lspsaga, lspsaga = pcall(require, "lspsaga")
if not ok_lspsaga then return end

local ok_colors, colors = pcall(require, 'tokyonight.colors')
if not ok_colors then return end

local u = require('user.utils')
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
