vim.pack.add({
	{ src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
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
		fzf = true,
		mini = {
			enabled = true,
		},
		neogit = true,
		noice = true,
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
			indent_scope_color = "maroon",
		},
		which_key = true,
	},
})

-- setup must be called before loading
vim.cmd.colorscheme("catppuccin-nvim")
