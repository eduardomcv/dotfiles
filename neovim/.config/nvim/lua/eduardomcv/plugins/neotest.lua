-- Test runner framework for neovim
--
-- This plugin is complaining about missing fields, when the fields aren's actually required.
---@diagnostic disable: missing-fields

return {
  "nvim-neotest/neotest",
  event = { 'BufReadPre', 'BufNewFile' },
  dependencies = {
    "nvim-lua/plenary.nvim",
    'nvim-treesitter/nvim-treesitter',
    "antoinemadec/FixCursorHold.nvim",
    -- Adapters
    'nvim-neotest/neotest-jest',
  },
  config = function()
    local neotest = require('neotest')
    local u = require('eduardomcv.utils')

    neotest.setup {
      adapters = {
        require('neotest-jest'),
      },
    }

    -- Keymaps
    u.nmap('<leader>tr', function()
      neotest.output_panel.open()
      neotest.run.run()
    end)
    u.nmap('<leader>tf', function()
      neotest.output_panel.open()
      neotest.run.run(vim.fn.expand('%'))
    end)
    u.nmap('<leader>td', function()
      neotest.run.run({ strategy = 'dap' })
    end)
    u.nmap('<leader>tt', neotest.output_panel.toggle)
  end
}
