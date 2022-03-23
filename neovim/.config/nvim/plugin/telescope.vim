nnoremap <C-p> :lua require('telescope.builtin').git_files()<cr>
nnoremap <leader>pp :lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>pf :lua require('telescope.builtin').grep_string()<cr>
nnoremap <leader>pg :lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>pb :lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>ph :lua require('telescope.builtin').help_tags()<cr>

lua << EOF
function telescope_buffer_dir()
  return vim.fn.expand('%:p:h')
end

local telescope = require('telescope')
local actions = require('telescope.actions')
local builtin = require('telescope.builtin')

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
    },
}

telescope.load_extension('fzy_native')
EOF
