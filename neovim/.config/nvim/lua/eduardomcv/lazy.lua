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
  'jose-elias-alvarez/null-ls.nvim',               -- Inject LSP diagnostics
  'jayp0521/mason-null-ls.nvim',                   -- Make it easier to use null-ls with mason

  'lewis6991/gitsigns.nvim',                       -- Git decorations
  { 'akinsho/git-conflict.nvim', version = "*" },  -- Git conflict visualizer

  {
    'rcarriga/nvim-dap-ui',   -- UI for debugging
    dependencies = {
      'mfussenegger/nvim-dap' -- Debugging
    }
  },
  {
    "mxsdev/nvim-dap-vscode-js", -- DAP support for vscode-js-debug
    dependencies = {
      "mfussenegger/nvim-dap"
    }
  },
  {
    'nvim-neotest/neotest', -- Test runner framework
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
      'antoinemadec/FixCursorHold.nvim',
      'haydenmeade/neotest-jest', -- Jest support
    }
  },

  'akinsho/nvim-bufferline.lua', -- Buffer line for tabs
  'mbbill/undotree',             -- Undo tree
  'gpanders/editorconfig.nvim',  -- EditorConfig support
  'uga-rosa/ccc.nvim',           -- Color picker and highlighter
  'folke/todo-comments.nvim',    -- Highlight keywords
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
