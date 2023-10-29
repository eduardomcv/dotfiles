-- Indentation guides

return {
  "lukas-reineke/indent-blankline.nvim",
  main = "ibl",
  event = { 'BufReadPre', 'BufNewFile' },
  config = function()
    vim.opt.list = true
    vim.opt.listchars:append("space:⋅")

    require('ibl').setup()
  end
}
