-- UI for LSP progress

return {
  "j-hui/fidget.nvim",
  tag = "legacy",
  event = "LspAttach",
  config = {
    require('fidget').setup {
      window = {
        blend = 0,
      },
    }
  }
}
