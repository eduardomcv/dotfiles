-- Buffer line for tabs

return {
  "akinsho/nvim-bufferline.lua",
  dependencies = {
    "catppuccin/nvim",
  },
  config = function()
    local u = require("eduardomcv.utils")

    require("bufferline").setup({
      options = {
        highlights = require("catppuccin.groups.integrations.bufferline").get(),
        mode = "tabs",
        always_show_bufferline = false,
        show_buffer_close_icons = false,
        show_close_icon = false,
        color_icons = true,
        diagnostics = "nvim_lsp",
      },
    })

    -- Tab management
    u.nmap("<leader>l", ":BufferLineCycleNext<CR>")
    u.nmap("<leader>h", ":BufferLineCyclePrev<CR>")
    u.nmap("<leader>tn", ":tabnew<CR>")
    u.nmap("<leader>to", ":tabonly<CR>")
    u.nmap("<leader>tc", ":tabclose<CR>")
    u.nmap("<leader>tm", ":tabmove<CR>")
  end,
}
