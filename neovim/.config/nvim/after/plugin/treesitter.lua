local ok, treesitter = pcall(require, 'nvim-treesitter.configs')
if not ok then return end

treesitter.setup {
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
