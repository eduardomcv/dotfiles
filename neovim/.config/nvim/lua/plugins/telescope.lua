if IS_VSCODE then
	-- Searching is handled by vscode
	return {}
end

-- Cache the results of "git rev-parse"
local is_inside_work_tree = {}

local function search_project_files()
	local cwd = vim.fn.getcwd()
	if is_inside_work_tree[cwd] == nil then
		vim.fn.system("git rev-parse --is-inside-work-tree")
		is_inside_work_tree[cwd] = vim.v.shell_error == 0
	end

	if is_inside_work_tree[cwd] then
		require("telescope.builtin").git_files()
	else
		require("telescope.builtin").find_files()
	end
end

local function search_config_files()
	require("telescope.builtin").find_files({
		cwd = vim.fn.stdpath("config"),
	})
end

return {
	"nvim-telescope/telescope.nvim",
	branch = "0.1.x",
	lazy = true,
	cmd = "Telescope",
	dependencies = {
		"nvim-lua/plenary.nvim",
		{
			"nvim-telescope/telescope-fzf-native.nvim",
			build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release",
		},
	},
	keys = {
		{ "<c-p>", search_project_files, desc = "Search project" },
		{ "<leader>sp", search_project_files, desc = "Search project" },
		{ "<leader>sf", "<cmd>Telescope find_files<cr>", desc = "Search files" },
		{ "<leader>sr", "<cmd>Telescope oldfiles<cr>", desc = "Search recent files" },
		{ "<leader>sc", search_config_files, desc = "Search config files" },
		{ "<leader>sg", "<cmd>Telescope live_grep<cr>", desc = "Live grep" },
		{ "<leader>sb", "<cmd>Telescope buffers<cr>", desc = "Search buffers" },
		{ "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Search help tags" },
	},
	config = function()
		local telescope = require("telescope")
		local actions = require("telescope.actions")

		telescope.setup({
			defaults = {
				sorting_strategy = "ascending",
				layout_strategy = "horizontal",
				layout_config = {
					prompt_position = "top",
					height = 0.6,
					width = 0.6,
					preview_width = 0.55,
					preview_cutoff = 130,
				},
				mappings = {
					n = {
						-- Quit with "q" in normal mode
						["q"] = actions.close,
					},
				},
			},
			pickers = {
				find_files = {
					hidden = true,
					file_ignore_patterns = { "^.git/" },
				},
				git_files = {
					hidden = true,
				},
				live_grep = {
					file_ignore_patterns = { "^.git/" },
					additional_args = { "--hidden" },
				},
				grep_string = {
					file_ignore_patterns = { "^.git/" },
					additional_args = { "--hidden" },
				},
				buffers = {
					mappings = {
						i = {
							["<c-d>"] = actions.delete_buffer + actions.move_to_top,
						},
						n = {
							["<c-d>"] = actions.delete_buffer + actions.move_to_top,
						},
					},
				},
			},
			extensions = {
				fzf = {},
			},
		})

		telescope.load_extension("fzf")
	end,
}
