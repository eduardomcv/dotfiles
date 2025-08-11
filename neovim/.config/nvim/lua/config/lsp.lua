-- The list of servers to enable
local lsp_servers = {
	"bashls",
	"eslint",
	"emmet_language_server",
	"jsonls",
	"lua_ls",
	"vtsls",
	"yamlls",
}

vim.lsp.enable(lsp_servers)

--- Keymaps
vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, { desc = "Code Action" })
vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Rename" })
vim.keymap.set({ "n", "v" }, "<leader>cl", vim.lsp.codelens.run, { desc = "Run Codelens" })
vim.keymap.set("n", "<leader>cL", vim.lsp.codelens.refresh, { desc = "Refresh & Display Codelens" })

-- Configure diagnostics
vim.diagnostic.config({
	underline = true,
	severity_sort = true,
	update_in_insert = false,
	virtual_text = {
		spacing = 4,
		source = "if_many",
		prefix = "●",
	},
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = " ",
			[vim.diagnostic.severity.WARN] = " ",
			[vim.diagnostic.severity.HINT] = " ",
			[vim.diagnostic.severity.INFO] = " ",
		},
	},
})
