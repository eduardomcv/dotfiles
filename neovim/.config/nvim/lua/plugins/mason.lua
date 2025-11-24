vim.pack.add({
	"https://github.com/mason-org/mason.nvim",
	"https://github.com/mason-org/mason-lspconfig.nvim",
})

require("mason").setup()

require("mason-lspconfig").setup({
	automatic_enable = true,
	ensure_installed = {
		"bashls",
		"lua_ls",
		"stylua",
		"vtsls",
		"emmet_language_server",
		"eslint",
		"jsonls",
		"cssls",
		"yamlls",
	},
})
