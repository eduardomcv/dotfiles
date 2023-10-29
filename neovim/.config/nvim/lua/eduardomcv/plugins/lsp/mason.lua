return {
  "williamboman/mason.nvim", -- Manage LSPs, linters, formatters
  dependencies = {
    "williamboman/mason-lspconfig.nvim", -- Make it easier to use lspconfig with mason
  },
  config = function()
    local servers = require("eduardomcv.plugins.lsp.config.server-configurations")

    require("mason").setup()

    require("mason-lspconfig").setup({
      automatic_installation = true,
      ensure_installed = vim.tbl_keys(servers),
    })
  end,
}
