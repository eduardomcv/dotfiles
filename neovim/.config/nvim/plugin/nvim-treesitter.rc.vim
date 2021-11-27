lua <<EOF
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
    disable = {},
  },
  indent = {
    enable = false,
    disable = {},
  },
  ensure_installed = {
    "typescript",
    "tsx",
    "javascript",
    "json",
    "yaml",
    "html",
    "scss",
    "css"
  },
}

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.tsx.used_by = { "typescript.tsx" }
EOF
