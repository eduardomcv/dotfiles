return {
	"catppuccin/nvim",
	lazy = false,
	priority = 1000,
	name = "catppuccin",
	config = function()
		require("catppuccin").setup({
			float = {
				solid = false,
				transparent = true,
			},
			integrations = {
				blink_cmp = {
					style = "bordered",
				},
				treesitter = true,
				which_key = true,
				neogit = true,
				diffview = true,
				snacks = {
					enabled = true,
				},
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
