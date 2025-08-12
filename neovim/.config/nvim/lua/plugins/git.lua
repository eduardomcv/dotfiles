return {
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
