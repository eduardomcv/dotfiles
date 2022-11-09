local status, FTerm = pcall(require, 'FTerm')
if not status then return end


local opts = { noremap = true, silent = true }

vim.keymap.set('n', '<leader>gg', function()
  FTerm.run('lazygit')
end, opts)
vim.keymap.set('t', '<C-d>', FTerm.exit, opts)
