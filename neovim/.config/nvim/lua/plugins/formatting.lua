return {
	{
		"stevearc/conform.nvim",
		event = { "BufWritePre" },
		cmd = { "ConformInfo" },
		dependencies = "mason.nvim",
		---@module "conform"
		---@type conform.setupOpts
		opts = {
			formatters_by_ft = {
				lua = { "stylua" },
				javascript = { "prettier" },
			},
			default_format_opts = {
				lsp_format = "fallback",
			},
			format_on_save = {
				timeout_ms = 500,
			},
		},
	},
	{
		"zapling/mason-conform.nvim",
		dependencies = {
			"mason.nvim",
			"conform.nvim",
		},
		config = function()
			require("mason-conform").setup()
		end,
	},
}
