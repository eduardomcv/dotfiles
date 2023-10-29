local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

local plugins = {
  'jose-elias-alvarez/null-ls.nvim', -- Inject LSP diagnostics
  'jayp0521/mason-null-ls.nvim',     -- Make it easier to use null-ls with mason
}

require('lazy').setup({
  { import = 'eduardomcv.plugins' },
  { import = 'eduardomcv.plugins.lsp' },
}, {
  install = {
    colorscheme = { 'catppuccin' },
  },
  checker = {
    enabled = true,
    notify = false,
  },
  change_detection = {
    notify = false,
  },
})
