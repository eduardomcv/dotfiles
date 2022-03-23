require('nvim-treesitter.configs').setup {
  highlight = {
    enable = true,
    disable = {},
  },
  indent = {
    enable = false,
    disable = {},
  },
  ensure_installed = {
    "lua",
    "typescript",
    "tsx",
    "javascript",
    "json",
    "yaml",
    "html",
    "scss",
    "css",
  },
}

local ft_to_parser = require"nvim-treesitter.parsers".filetype_to_parsername
ft_to_parser.tsx = "typescript.tsx"
