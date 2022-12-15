local ok, ccc = pcall(require, 'ccc')
if not ok then return end

ccc.setup {
  highlighter = {
    auto_enable = true
  }
}

vim.keymap.set('n', '<leader>cp', ':CccPick<CR>')
vim.keymap.set('n', '<leader>cc', ':CccConvert<CR>')
