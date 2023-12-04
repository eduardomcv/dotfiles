-- File browser

return {
  "nvim-tree/nvim-tree.lua",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    -- Disable netrw
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1

    local u = require("eduardomcv.utils")

    require("nvim-tree").setup({
      disable_netrw = true,
      diagnostics = {
        enable = true,
      },
      view = {
        width = 30,
      },
    })

    -- Keymaps
    u.nmap("<leader>b", ":NvimTreeFindFileToggle<CR>")
  end,
}
