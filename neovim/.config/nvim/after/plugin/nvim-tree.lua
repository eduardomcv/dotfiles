local ok, nvim_tree = pcall(require, 'nvim-tree')
if not ok then return end

local utils = require('user.utils')

-- Disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

nvim_tree.setup({
  disable_netrw = true,
  diagnostics = {
    enable = true,
  },
  view = {
    width = 40,
  },
})

-- Keymaps
utils.nmap('<leader>b', ':NvimTreeFindFileToggle<CR>')
