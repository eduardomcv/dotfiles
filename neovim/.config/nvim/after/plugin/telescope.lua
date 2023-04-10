local ok, telescope = pcall(require, 'telescope')
if not ok then return end

local utils = require('user.utils')

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
    layout_strategy = "vertical",
    layout_config = {
      vertical = {
        prompt_position = "top",
        preview_height = 0.35,
        width = {
          0.5,
          max = 200,
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
      show_untracked = true
    }
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = false,
      override_file_sorter = true,
    },
  },
}

telescope.load_extension('fzy_native')

-- Builtin maps
utils.nmap('<C-p>', find_project_files)
utils.nmap('<C-f>', builtin.current_buffer_fuzzy_find)
utils.nmap('<C-f>', function()
  local text = getVisualSelection()
  builtin.current_buffer_fuzzy_find({ default_text = text })
end)
utils.nmap('<leader>sf', builtin.find_files)
utils.nmap('<leader>sw', builtin.grep_string)
utils.nmap('<leader>sg', builtin.live_grep)
utils.nmap('<leader>sg', function()
  local text = getVisualSelection()
  builtin.live_grep({ default_text = text })
end)
utils.nmap('<leader>sb', builtin.buffers)
utils.nmap('<leader>sh', builtin.help_tags)
utils.nmap('<leader>sr', builtin.oldfiles)
utils.nmap('<leader>st', builtin.tags)
