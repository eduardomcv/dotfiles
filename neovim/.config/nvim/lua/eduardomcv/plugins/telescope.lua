-- Fuzzy finder

return {
  'nvim-telescope/telescope.nvim',
  branch = '0.1.x',
  dependencies = {
    'nvim-lua/plenary.nvim',                    -- Common utilities
    'nvim-telescope/telescope-fzy-native.nvim', -- Compiled FZY style sorter
    'debugloop/telescope-undo.nvim'             -- Visualize and fuzzy search undo tree with telescope
  },
  config = function()
    local telescope = require('telescope')
    local u = require('eduardomcv.utils')

    local builtin = require('telescope.builtin')
    local actions = require('telescope.actions')
    local trouble = require("trouble.providers.telescope")

    local function find_project_files()
      local opts = {}
      local in_git_repo = vim.fn.systemlist "git rev-parse --is-inside-work-tree"[1] == 'true'

      if in_git_repo then
        builtin.git_files(opts)
      else
        builtin.find_files(opts)
      end
    end

    local function getVisualSelection()
      vim.cmd('noau normal! "vy"')
      local text = vim.fn.getreg('v')
      vim.fn.setreg('v', {})

      text = string.gsub(text, "\n", "")
      if #text > 0 then
        return text
      else
        return ''
      end
    end

    telescope.setup {
      defaults = {
        path_display = { "truncate" },
        sorting_strategy = "ascending",
        layout_strategy = "horizontal",
        layout_config = {
          horizontal = {
            prompt_position = "top",
            preview_cutoff = 200,
            preview_width = 0.55,
            height = 0.7,
            width = {
              0.7,
              min = 80
            }
          }
        },
        file_ignore_patterns = { '^.git/', '^node_modules/' },
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
        mappings = {
          n = {
            ["q"] = actions.close,
            ["<c-t>"] = trouble.open_with_trouble,
          },
          i = {
            ["<c-t>"] = trouble.open_with_trouble,
          },
        },
      },
      pickers = {
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
              ['x'] = 'delete_buffer'
            }
          }
        }
      },
      extensions = {
        fzy_native = {
          override_generic_sorter = false,
          override_file_sorter = true,
        },
      },
    }

    -- Telescope extensions
    telescope.load_extension('fzy_native')
    telescope.load_extension("undo")

    -- Builtin maps
    u.nmap('<C-p>', find_project_files)
    u.nmap('<C-f>', builtin.current_buffer_fuzzy_find)
    u.nmap('<C-f>', function()
      local text = getVisualSelection()
      builtin.current_buffer_fuzzy_find({ default_text = text })
    end)
    u.nmap('<leader>sw', builtin.grep_string)
    u.nmap('<leader>sg', builtin.git_status)
    u.nmap('<leader>ss', builtin.live_grep)
    u.vmap('<leader>ss', function()
      local text = getVisualSelection()
      builtin.live_grep({ default_text = text })
    end)
    u.nmap('<leader>sb', builtin.buffers)
    u.nmap('<leader>sh', builtin.help_tags)
    u.nmap('<leader>sr', builtin.oldfiles)
    u.nmap('<leader>st', builtin.tags)
    u.nmap('<leader>su', ':Telescope undo')
  end
}
