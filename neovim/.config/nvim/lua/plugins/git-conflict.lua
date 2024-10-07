return {
  "akinsho/git-conflict.nvim",
  version = "*",
  config = function()
    require("git-conflict").setup({
      debug = false,
      default_mappings = true,
      default_commands = true,
      disable_diagnostics = false,
      list_opener = "copen",
      highlights = {
        incoming = "DiffAdd",
        current = "DiffText",
      },
    })

    vim.keymap.set("n", "<leader>gc", function()
      vim.cmd("GitConflictListQf")
    end, { desc = " Git Conflict Quickfix List" })
  end,
}
