return {
  "neovim/nvim-lspconfig",
  opts = {
    inlay_hints = { enabled = false },
    servers = {
      cssls = {},
      stylelint_lsp = {
        settings = {
          stylelintplus = {
            autoFixOnSave = true,
          },
        },
      },
    },
  },
}
