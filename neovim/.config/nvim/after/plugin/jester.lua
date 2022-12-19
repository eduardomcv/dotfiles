local ok, jester = pcall(require, 'jester')
if not ok then return end

jester.setup {
  path_to_jest_run = './node_modules/.bin/jest',
}

vim.keymap.set('n', '<leader>tr', jester.run, {})
vim.keymap.set('n', '<leader>tf', jester.run_file, {})
vim.keymap.set('n', '<leader>td', jester.debug, {})
