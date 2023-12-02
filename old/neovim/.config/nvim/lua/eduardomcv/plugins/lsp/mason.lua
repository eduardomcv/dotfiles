return {
  "williamboman/mason.nvim", -- Manage LSPs, linters, formatters
  dependencies = {
    "williamboman/mason-lspconfig.nvim", -- Make it easier to use lspconfig with mason
    "WhoIsSethDaniel/mason-tool-installer.nvim", -- Auto install linters, formatters and other tools
  },
  config = function()
    local servers = require("eduardomcv.plugins.lsp.config.server-configurations")

    require("mason").setup()

    require("mason-lspconfig").setup({
      automatic_installation = true,
      ensure_installed = vim.tbl_keys(servers),
    })

    require("mason-tool-installer").setup({
      ensure_installed = {
        "prettierd",
        "eslint_d",
        "stylua",
      },
    })
  end,
}
