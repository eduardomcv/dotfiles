return {
	-- Better UI for messages, cmdline and popupmenu
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		dependencies = {
			-- UI component library
			{ "MunifTanjim/nui.nvim", lazy = true },
		},
		opts = {
			presets = {
				bottom_search = false,
				command_palette = true,
				long_message_to_split = true,
				lsp_doc_border = true,
			},
			lsp = {
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
				},
				progress = {
					view = "notify",
				},
			},
			views = {
				cmdline_popup = {
					position = {
						row = "20%",
					},
				},
				notify = {
					replace = true,
				},
			},
			routes = {
				{
					view = "notify",
					filter = { event = "msg_showmode" },
				},
			},
		},
	},
	-- Show pop-up with available keybindings
	{
		"folke/which-key.nvim",
		dependencies = {
			"nvim-mini/mini.nvim",
		},
		event = "VeryLazy",
		opts = {
			spec = {
				{ "<leader>c", group = "+code" },
				{ "<leader>f", group = "+format" },
				{ "<leader>g", group = "+git" },
				{ "<leader>n", group = "+notification" },
				{ "<leader>s", group = "+search" },
			},
		},
	},
	-- Git conflicts visualizer
	{ "akinsho/git-conflict.nvim", version = "*", config = true },
	-- Improve tab integration
	{
		"nanozuki/tabby.nvim",
		config = function()
			require("tabby").setup()

			local function rename_tab()
				local tab_name = vim.fn.input({ prompt = "New tab name" })
				if tab_name ~= "" then
					require("tabby").tab_rename(tab_name)
				end
			end

			vim.keymap.set("n", "<leader>tr", rename_tab, { desc = "Rename tab" })
		end,
	},
	-- Highlight TODO comments
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {},
	},
}
