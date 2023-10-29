return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
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
        'vimdoc'
      },
    }
  end
}
