vim.pack.add({
	-- Package manager for LSP servers, DAP servers, linters and formatters
	"https://github.com/mason-org/mason.nvim",
	-- Automatically install Mason packages
	"https://github.com/owallb/mason-auto-install.nvim",
})

require("mason").setup({})

require("mason-auto-install").setup({
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
})
