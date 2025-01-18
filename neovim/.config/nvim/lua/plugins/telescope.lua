-- Cache the results of "git rev-parse"
local is_inside_work_tree = {}

local function find_project_files()
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
		{ "<c-p>", find_project_files, desc = "Search project" },
		{ "<leader>sp", find_project_files, desc = "Search project" },
		{ "<leader>sf", "<cmd>Telescope find_files<cr>", desc = "Search files" },
		{ "<leader>sg", "<cmd>Telescope live_grep<cr>", desc = "Live grep" },
		{ "<leader>sb", "<cmd>Telescope buffers<cr>", desc = "Search buffers" },
		{ "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Search help tags" },
	},
	config = function()
		local telescope = require("telescope")
		local actions = require("telescope.actions")

		telescope.setup({
			defaults = {
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
					find_command = { "fd", "--type", "f", "--strip-cwd-prefix", "-H", "-E", ".git" },
				},
				git_files = {
					hidden = true,
				},
				live_grep = {
					additional_args = { "--hidden" },
				},
				grep_string = {
					additional_args = { "--hidden" },
				},
			},
		})

		telescope.load_extension("fzf")
	end,
}
