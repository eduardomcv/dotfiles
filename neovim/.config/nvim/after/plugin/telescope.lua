local ok, telescope = pcall(require, 'telescope')
if not ok then return end

local builtin = require('telescope.builtin')
local actions = require('telescope.actions')
local file_browser = telescope.extensions.file_browser

local function telescope_buffer_dir()
  return vim.fn.expand('%:p:h')
end

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
      },
    },
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = false,
      override_file_sorter = true,
    },
    file_browser = {
      theme = "dropdown",
      -- disables netrw and use telescope-file-browser in its place
      hijack_netrw = true,
      mappings = {
        -- your custom insert mode mappings
        ["i"] = {
          ["<C-w>"] = function() vim.cmd('normal vbd') end,
        },
        -- your custom normal mode mappings
        ["n"] = {
          ["N"] = file_browser.actions.create,
          ["h"] = file_browser.actions.goto_parent_dir,
          ["/"] = function()
            vim.cmd('startinsert')
          end
        },
      },
    },
  },
}

telescope.load_extension('fzy_native')
telescope.load_extension('file_browser')
telescope.load_extension('aerial')

local opts = { noremap = true, silent = true }

-- Builtin maps
vim.keymap.set('n', '<C-p>', find_project_files, opts)
vim.keymap.set('n', '<C-f>', builtin.current_buffer_fuzzy_find, opts)
vim.keymap.set('v', '<C-f>', function()
  local text = getVisualSelection()
  builtin.current_buffer_fuzzy_find({ default_text = text })
end, opts)
vim.keymap.set('n', '<leader>ff', builtin.find_files, opts)
vim.keymap.set('n', '<leader>fw', builtin.grep_string, opts)
vim.keymap.set('n', '<leader>fg', builtin.live_grep, opts)
vim.keymap.set('v', '<leader>fg', function()
  local text = getVisualSelection()
  builtin.live_grep({ default_text = text })
end, opts)
vim.keymap.set('n', '<leader>fb', builtin.buffers, opts)
vim.keymap.set('n', '<leader>fh', builtin.help_tags, opts)

-- File browser maps
vim.keymap.set('n', '<leader>b', function()
  file_browser.file_browser({
    path = "%:p:h",
    cwd = telescope_buffer_dir(),
    respect_gitignore = false,
    hidden = true,
    grouped = true,
    previewer = false,
    initial_mode = "normal",
    layout_config = { height = 40 }
  })
end, opts)

-- Aerial maps
vim.keymap.set('n', '<leader>o', ':Telescope aerial<CR>', opts)
