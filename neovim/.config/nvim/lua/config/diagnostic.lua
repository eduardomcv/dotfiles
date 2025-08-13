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

-- Keymaps
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Jump to previous diagnostic" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Jump to next diagnostic" })
