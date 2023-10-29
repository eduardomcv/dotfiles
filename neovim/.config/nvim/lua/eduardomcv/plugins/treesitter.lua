return {
  "nvim-treesitter/nvim-treesitter",
  dependencies = {
    'windwp/nvim-ts-autotag' -- Auto close and auto rename tags
  },
  build = ":TSUpdate",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    require("nvim-treesitter.configs").setup {
      highlight = {
        enable = true,
      },
      indent = {
        enable = true,
      },
      autotag = {
        enable = true
      },
      ensure_installed = {
        'lua',
        'javascript',
        'typescript',
        'tsx',
        'html',
        'css',
        'scss',
        'graphql',
        'json',
        'yaml',
        'markdown',
        'markdown_inline',
        'vim',
        'vimdoc',
        'gitignore',
        'bash',
      },
    }
  end
}
