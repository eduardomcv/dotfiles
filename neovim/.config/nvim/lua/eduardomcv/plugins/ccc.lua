-- Color picker and highlighter

return {
  'uga-rosa/ccc.nvim',
  event = { 'BufReadPre', 'BufNewFile' },
  config = function()
    local u = require('eduardomcv.utils')

    require('ccc').setup {
      highlighter = {
        auto_enable = true
      }
    }

    u.nmap('<leader>cp', ':CccPick<CR>')
    u.nmap('<leader>cc', ':CccConvert<CR>')
  end
}
