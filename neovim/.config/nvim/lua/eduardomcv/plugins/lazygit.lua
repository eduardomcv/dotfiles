return {
  'kdheepak/lazygit.nvim',
  config = function()
    local u = require('eduardomcv.utils')
    u.nmap('<leader>gg', ':LazyGit<cr>')
  end
}
