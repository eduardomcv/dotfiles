-- Disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require("nvim-tree").setup({
  disable_netrw = true,
  diagnostics = {
    enable = true,
  }
})

vim.keymap.set('n', '<leader>b', ':NvimTreeToggle<CR>')
