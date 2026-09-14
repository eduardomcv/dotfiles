use({
	{
		"mrcjkb/rustaceanvim",
		version = vim.version.range("^9"),
	},
})

vim.g.rustaceanvim = {
	server = {
		on_attach = function(_, bufnr)
			-- Support rust-analyzer's grouping
			vim.keymap.set("n", "<leader>ca", function()
				vim.cmd.RustLsp("codeAction")
			end, { silent = true, buffer = bufnr, desc = "Code Action" })

			-- Override Neovim's built-in hover keymap with rustaceanvim's hover actions
			vim.keymap.set("n", "K", function()
				vim.cmd.RustLsp({ "hover", "actions" })
			end, { silent = true, buffer = bufnr })
		end,
	},
}
