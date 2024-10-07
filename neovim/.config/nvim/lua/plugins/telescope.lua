return {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    {
      "debugloop/telescope-undo.nvim", -- Visualize and fuzzy search undo tree with telescope
      config = function()
        require("telescope").load_extension("undo")
      end,
    },
  },
  keys = {
    { "<leader>gc", false },
    { "<leader>gC", "<cmd>Telescope git_commits<CR>", desc = "Commits" },
    {
      "<c-p>",
      function()
        local builtin = require("telescope.builtin")
        local in_git_repo = vim.fn.systemlist("git rev-parse --is-inside-work-tree")[1] == "true"

        if in_git_repo then
          builtin.git_files()
        else
          builtin.find_files()
        end
      end,
      desc = "Find Project Files",
    },
    {
      "<leader>fu",
      function()
        require("telescope").extensions.undo.undo()
      end,
      desc = "Browse undo tree",
    },
  },
  opts = {
    defaults = {
      path_display = { "truncate" },
      sorting_strategy = "ascending",
      layout_strategy = "horizontal",
      layout_config = {
        prompt_position = "top",
        preview_cutoff = 150,
        preview_width = 0.42,
        height = 0.80,
        width = {
          0.80,
          min = 80,
        },
      },
    },
    pickers = {
      git_files = {
        show_untracked = true,
      },
      buffers = {
        mappings = {
          n = {
            ["x"] = "delete_buffer",
          },
        },
      },
    },
  },
}
