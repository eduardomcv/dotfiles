return {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    {
      "nvim-telescope/telescope-fzf-native.nvim", -- Compiled fzf style sorter
      build = "make",
      config = function()
        require("telescope").load_extension("fzf")
      end,
    },
    {
      "debugloop/telescope-undo.nvim", -- Visualize and fuzzy search undo tree with telescope
      config = function()
        require("telescope").load_extension("undo")
      end,
    },
  },
  build = "make",
  keys = {
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
  config = function(_, opts)
    local telescope = require("telescope")

    opts.defaults = vim.tbl_deep_extend("force", opts.defaults, {
      file_ignore_patterns = { "^.git/", "^node_modules/" },
      path_display = { "truncate" },
      sorting_strategy = "ascending",
      winblend = 0,
      layout_strategy = "horizontal",
      layout_config = {
        prompt_position = "top",
        preview_cutoff = 150,
        preview_width = 0.55,
        height = 0.7,
        width = {
          0.7,
          min = 80,
        },
      },
      vimgrep_arguments = {
        "rg",
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
        "--smart-case",
        "--trim",
        "--hidden",
      },
    })

    opts.pickers = {
      find_files = {
        hidden = true,
      },
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
    }

    telescope.setup(opts)
  end,
}
