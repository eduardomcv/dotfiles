local ok, jester = pcall(require, 'jester')
if not ok then return end

local utils = require('user.utils')

jester.setup {
  path_to_jest_run = './node_modules/.bin/jest',
}

utils.nmap('<leader>tr', jester.run)
utils.nmap('<leader>tf', jester.run_file)
utils.nmap('<leader>td', jester.debug)
