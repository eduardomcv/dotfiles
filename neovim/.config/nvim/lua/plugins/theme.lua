vim.pack.add({
	{ src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
})

require("catppuccin").setup({
	float = {
		solid = false,
		transparent = true,
	},
	lsp_styles = {
		underlines = {
			errors = { "undercurl" },
			hints = { "undercurl" },
			warnings = { "undercurl" },
			information = { "undercurl" },
			okay = { "undercurl" },
		},
	},
	auto_integrations = true,
	integrations = {
		snacks = {
			enabled = true,
			indent_scope_color = "maroon",
		},
		which_key = true,
	},
})

-- setup must be called before loading
vim.cmd.colorscheme("catppuccin-nvim")
