-- LSP server configurations

return {
  cssls = {},
  html = {},
  jsonls = {},
  emmet_ls = {},
  eslint = {},
  graphql = {},
  tsserver = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
      diagnostics = {
        globals = { 'vim' },
      },
    },
  }
}
