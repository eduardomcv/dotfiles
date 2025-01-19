return {
	"catppuccin/nvim",
	lazy = false,
	priority = 1000,
	name = "catppuccin",
	config = function()
		require("catppuccin").setup({
			integrations = {
				blink_cmp = true,
				telescope = true,
				treesitter = true,
				which_key = true,
				neogit = true,
				diffview = true,
				snacks = true,
				mini = {
					enabled = true,
				},
				native_lsp = {
					enabled = true,
					underlines = {
						errors = { "undercurl" },
						hints = { "undercurl" },
						warnings = { "undercurl" },
						information = { "undercurl" },
					},
				},
			},
		})

		-- Must load theme after calling setup
		vim.cmd.colorscheme("catppuccin-mocha")
	end,
}
