vim.pack.add({ "https://github.com/stevearc/conform.nvim" })

require("conform").setup({
	formatters_by_ft = {
		lua = { "stylua" },
		javascript = { "prettier" },
		typescript = { "prettier" },
		typescriptreact = { "prettier" },
		css = { "prettier" },
		scss = { "prettier" },
		md = { "prettier" },
		astro = { "prettier" },
		json = { "prettier" },
		yaml = { "prettier" },
		sh = { "shfmt" },
		zsh = { "shfmt" },
		bash = { "shfmt" },
		rust = { "rustfmt" },
		python = {
			"ruff_fix",
			"ruff_format",
			"ruff_organize_imports",
		},
	},
	default_format_opts = {
		lsp_format = "fallback",
	},
	format_on_save = function(bufnr)
		-- Disable with a global or buffer-local variable
		if vim.g.disable_format_on_save or vim.b[bufnr].disable_format_on_save then
			return
		end
		return { timeout_ms = 500, lsp_format = "fallback" }
	end,
})

vim.api.nvim_create_user_command("FormatOnSaveToggle", function(args)
	if args.bang then
		-- FormatOnSaveToggle! will toggle format-on-save just for the current buffer
		vim.b.disable_format_on_save = not vim.b.disable_format_on_save
	else
		vim.g.disable_format_on_save = not vim.g.disable_format_on_save
	end
end, {
	desc = "Toggle format-on-save",
	bang = true,
})

vim.keymap.set("n", "<leader>cf", function()
	require("conform").format({ async = true })
end, { desc = "Format buffer" })

vim.keymap.set("n", "<leader>fb", function()
	require("conform").format({ async = true })
end, { desc = "Format buffer" })

vim.keymap.set("n", "<leader>ff", ":FormatToggle<CR>", {
	desc = "Toggle format-on-save (global)",
})

vim.keymap.set("n", "<leader>ff", ":FormatToggle!<CR>", {
	desc = "Toggle format-on-save (buffer-local)",
})
