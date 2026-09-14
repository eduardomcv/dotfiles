use({ "stevearc/conform.nvim" })

local function has_local_node_modules_exe(cmd)
	return function(self, ctx)
		local resolve = require("conform.util").from_node_modules(cmd)
		---@diagnostic disable-next-line: redundant-parameter
		return resolve(self, ctx) ~= cmd
	end
end

local js_formatters = {
	"oxfmt",
	"prettier",
	stop_after_first = true,
}

require("conform").setup({
	formatters = {
		oxfmt = {
			condition = has_local_node_modules_exe("oxfmt"),
		},
	},
	formatters_by_ft = {
		lua = { "stylua" },
		javascript = js_formatters,
		javascriptreact = js_formatters,
		typescript = js_formatters,
		typescriptreact = js_formatters,
		css = js_formatters,
		scss = js_formatters,
		markdown = js_formatters,
		json = js_formatters,
		yaml = js_formatters,
		astro = { "prettier" },
		sh = { "shfmt" },
		zsh = { "shfmt" },
		bash = { "shfmt" },
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

local function format_buffer()
	require("conform").format({ async = true })
end

vim.keymap.set("n", "<leader>fb", format_buffer, { desc = "Format buffer" })
vim.keymap.set("n", "<leader>cf", format_buffer, { desc = "Format buffer" })

vim.keymap.set("n", "<leader>fF", ":FormatOnSaveToggle<CR>", {
	desc = "Toggle format-on-save (global)",
})

vim.keymap.set("n", "<leader>ff", ":FormatOnSaveToggle!<CR>", {
	desc = "Toggle format-on-save (buffer-local)",
})
