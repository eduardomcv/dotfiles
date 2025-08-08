return {
	"stevearc/conform.nvim",
	event = { "BufWritePre" },
	cmd = { "ConformInfo" },
	---@module "conform"
	---@type conform.setupOpts
	opts = {
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
		},
		default_format_opts = {
			lsp_format = "fallback",
		},
		format_on_save = {
			timeout_ms = 500,
		},
	},
	keys = {
		{
			"<leader>cf",
			function()
				require("conform").format({ async = true })
			end,
			desc = "Format buffer",
		},
	},
	init = function()
		vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
	end,
}
