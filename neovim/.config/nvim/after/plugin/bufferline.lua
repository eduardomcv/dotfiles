local ok, bufferline = pcall(require, "bufferline")
if not ok then return end

bufferline.setup({
  options = {
    highlights = require("catppuccin.groups.integrations.bufferline").get(),
    mode = "tabs",
    always_show_bufferline = false,
    show_buffer_close_icons = false,
    show_close_icon = false,
    color_icons = true,
    diagnostics = 'nvim_lsp',
  },
})

vim.keymap.set('n', '<leader>tn', ':BufferLineCycleNext<CR>')
vim.keymap.set('n', '<leader>tp', ':BufferLineCyclePrev<CR>')
