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

local opts = { noremap = true, silent = true }

local function nmap(lhs, rhs)
  vim.keymap.set('n', lhs, rhs, opts)
end

local function vmap(lhs, rhs)
  vim.keymap.set('v', lhs, rhs, opts)
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

-- Builtin maps
nmap('<C-p>', find_project_files)
nmap('<C-f>', builtin.current_buffer_fuzzy_find)
vmap('<C-f>', function()
  local text = getVisualSelection()
  builtin.current_buffer_fuzzy_find({ default_text = text })
end)
nmap('<leader>sf', builtin.find_files)
nmap('<leader>sw', builtin.grep_string)
nmap('<leader>sg', builtin.live_grep)
vmap('<leader>sg', function()
  local text = getVisualSelection()
  builtin.live_grep({ default_text = text })
end)
nmap('<leader>sb', builtin.buffers)
nmap('<leader>sh', builtin.help_tags)

-- File browser maps
nmap('<leader>b', function()
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
end)

-- Aerial maps
nmap('<leader>o', ':Telescope aerial<CR>')
