local ok, trouble = pcall(require, 'trouble')
if not ok then return end

local utils = require('eduardomcv.utils')

-- Setup
trouble.setup {
  auto_open = true,
  auto_close = true,
}

-- Mappings
utils.nmap('<leader>xx', '<cmd>TroubleToggle<cr>')
utils.nmap('<leader>xw', '<cmd>TroubleToggle workspace_diagnostics<cr>')
utils.nmap('<leader>xd', '<cmd>TroubleToggle document_diagnostics<cr>')
utils.nmap('<leader>xl', '<cmd>TroubleToggle loclist<cr>')
utils.nmap('<leader>xq', '<cmd>TroubleToggle quickfix<cr>')
utils.nmap('gR', '<cmd>TroubleToggle lsp_references<cr>')

-- Create autocmd to replace quickfix with Trouble
local function replace_quickfix_with_trouble()
  -- Check whether we deal with a quickfix or location list buffer, close the window and open the
  -- corresponding Trouble window instead.
  if vim.fn.getloclist(0, { filewinid = 1 }).filewinid ~= 0 then
    vim.defer_fn(function()
      vim.cmd.lclose()
      trouble.open("loclist")
    end, 0)
  else
    vim.defer_fn(function()
      vim.cmd.cclose()
      trouble.open("quickfix")
    end, 0)
  end
end

local group = vim.api.nvim_create_augroup("ReplaceQuickfixWithTrouble", {})

vim.api.nvim_create_autocmd("BufWinEnter", {
  pattern = "quickfix",
  group = group,
  callback = replace_quickfix_with_trouble,
})

