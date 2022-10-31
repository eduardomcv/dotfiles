local status, bufferline = pcall(require, "bufferline")
if (not status) then return end

local vs_colors = require('vscode.colors')

bufferline.setup({
  options = {
    mode = "tabs",
    separator_style = 'slant',
    always_show_bufferline = false,
    show_buffer_close_icons = false,
    show_close_icon = false,
    color_icons = true
  },
  highlights = {
    separator = {
      fg = vs_colors.vscTabOutside,
    },
    separator_selected = {
      fg = vs_colors.vscTabOutside
    }
  },
})

vim.keymap.set('n', '<leader>tk', ':BufferLineCycleNext<CR>', {})
vim.keymap.set('n', '<leader>tj', ':BufferLineCyclePrev<CR>', {})
