local ok, neotest = pcall(require, 'neotest');
if not ok then return end

local utils = require('user.utils')

neotest.setup {
  adapters = {
    require('neotest-jest'),
  },
}

-- Keymaps
utils.nmap('<leader>tr', function ()
  neotest.output_panel.open()
  neotest.run.run()
end)
utils.nmap('<leader>tf', function ()
  neotest.output_panel.open()
  neotest.run.run(vim.fn.expand('%'))
end)
utils.nmap('<leader>td', function ()
  neotest.run.run({ strategy = 'dap' })
end)
utils.nmap('<leader>tt', neotest.output_panel.toggle)
