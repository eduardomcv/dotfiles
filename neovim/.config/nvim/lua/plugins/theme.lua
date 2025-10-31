vim.pack.add({
	{
		src = "https://github.com/catppuccin/nvim",
		name = "catppuccin",
	},
})

require("catppuccin").setup({
	float = {
		solid = false,
		transparent = true,
	},
	integrations = {
		blink_cmp = {
			style = "bordered",
		},
		mason = true,
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
		snacks = {
			enabled = true,
		},
		treesitter = true,
		which_key = true,
	},
})

-- Must load theme after calling setup
vim.cmd.colorscheme("catppuccin-mocha")
