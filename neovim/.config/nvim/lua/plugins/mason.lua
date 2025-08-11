return {
	-- Package manager for LSP servers, DAP servers, linters and formatters
	{
		"mason-org/mason.nvim",
		opts = {},
	},
	-- Automatically install Mason packages
	{
		"owallb/mason-auto-install.nvim",
		dependencies = {
			"mason-org/mason.nvim",
		},
		opts = {
			packages = {
				"bash-language-server",
				"eslint-lsp",
				"emmet-language-server",
				"json-lsp",
				"lua-language-server",
				"vtsls",
				"yaml-language-server",
				"stylua",
				"prettier",
				"shfmt",
			},
		},
	},
}
