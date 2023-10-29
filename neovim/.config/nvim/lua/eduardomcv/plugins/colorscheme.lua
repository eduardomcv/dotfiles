-- Catpuccin Theme

return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  config = function()
    require("catppuccin").setup({
      flavour = "mocha",
      no_italic = true,
      integrations = {
        fidget = true,
        indent_blankline = {
          enabled = true,
        },
        dap = {
          enabled = true,
          enable_ui = true,
        },
        native_lsp = {
          enabled = true,
          underlines = {
            errors = { "underline" },
            hints = { "underline" },
            warnings = { "underline" },
            information = { "underline" },
          },
        },
        mason = true,
        neotest = true,
        cmp = true,
        nvimtree = true,
        treesitter = true,
        telescope = true,
        lsp_trouble = true,
        gitsigns = true,
      },
    })

    vim.cmd.colorscheme("catppuccin")
  end,
}
