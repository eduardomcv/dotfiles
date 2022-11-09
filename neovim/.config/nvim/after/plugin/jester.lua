local jester = require('jester')

vim.keymap.set('n', '<leader>tr', jester.run, {})
vim.keymap.set('n', '<leader>tf', jester.run_file, {})
vim.keymap.set('n', '<leader>td', jester.debug, {})
