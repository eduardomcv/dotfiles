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
		dependencies = "mini.icons",
		event = "VeryLazy",
		opts = {
			spec = {
				{ "<leader>s", group = "+search" },
				{ "<leader>g", group = "+git" },
				{ "<leader>c", group = "+code" },
				{ "<leader>n", group = "+notification" },
			},
		},
	},
	-- Git UI similar to magit
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"folke/snacks.nvim",
			{
				"sindrets/diffview.nvim",
				opts = {
					view = {
						merge_tool = {
							layout = "diff3_mixed",
						},
					},
				},
			},
		},
		opts = {
			integrations = {
				diffview = true,
				snacks = true,
			},
		},
		keys = {
			{
				"<leader>gg",
				function()
					require("neogit").open()
				end,
				desc = "Open neogit",
			},
			{
				"<leader>gs",
				function()
					require("neogit").open({ kind = "split" })
				end,
				desc = "Open neogit (split window)",
			},
			{
				"<leader>gv",
				function()
					require("neogit").open({ kind = "vsplit" })
				end,
				desc = "Open neogit (vertical split window)",
			},
		},
	},
}
