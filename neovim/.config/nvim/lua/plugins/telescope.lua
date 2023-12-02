local function getVisualSelection()
  vim.cmd('noau normal! "vy"')
  local text = vim.fn.getreg("v")
  vim.fn.setreg("v", {})

  text = string.gsub(text, "\n", "")
  if #text > 0 then
    return text
  else
    return ""
  end
end

return {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    "nvim-telescope/telescope-fzy-native.nvim", -- Compiled FZY style sorter
    "debugloop/telescope-undo.nvim", -- Visualize and fuzzy search undo tree with telescope
  },
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
  },
  config = function(_, opts)
    local telescope = require("telescope")

    opts.defaults = vim.tbl_deep_extend("force", opts.defaults, {
      path_display = { "truncate" },
      sorting_strategy = "ascending",
      layout_strategy = "horizontal",
      layout_config = {
        prompt_position = "top",
        preview_cutoff = 200,
        preview_width = 0.55,
        height = 0.7,
        width = {
          0.7,
          min = 80,
        },
      },
      winblend = 0,
      file_ignore_patterns = { "^.git/", "^node_modules/" },
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
      current_buffer_fuzzy_find = {
        previewer = false,
      },
      buffers = {
        mappings = {
          n = {
            ["x"] = "delete_buffer",
          },
        },
      },
    }

    opts.extensions = {
      fzy_native = {
        override_generic_sorter = false,
        override_file_sorter = true,
      },
    }

    telescope.load_extension("fzy_native")
    telescope.load_extension("undo")
    telescope.setup(opts)
  end,
}
