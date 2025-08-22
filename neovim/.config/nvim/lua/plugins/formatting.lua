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
		format_on_save = function(bufnr)
			-- Disable with a global or buffer-local variable
			if vim.g.disable_format_on_save or vim.b[bufnr].disable_format_on_save then
				return
			end
			return { timeout_ms = 500, lsp_format = "fallback" }
		end,
	},
	keys = {
		{
			"<leader>cf",
			function()
				require("conform").format({ async = true })
			end,
			desc = "Format buffer",
		},
		{
			"<leader>fb",
			function()
				require("conform").format({ async = true })
			end,
			desc = "Format buffer",
		},
		{
			"<leader>fd",
			":FormatDisable<cr>",
			desc = "Disable format-on-save",
		},
		{
			"<leader>fe",
			":FormatEnable<cr>",
			desc = "Enable format-on-save",
		},
	},
	init = function()
		vim.api.nvim_create_user_command("FormatDisable", function(args)
			if args.bang then
				-- FormatDisable! will disable format-on-save just for the current buffer
				vim.b.disable_format_on_save = true
			else
				vim.g.disable_format_on_save = true
			end
		end, {
			desc = "Disable format-on-save",
			bang = true,
		})

		vim.api.nvim_create_user_command("FormatEnable", function()
			vim.b.disable_format_on_save = false
			vim.g.disable_format_on_save = false
		end, {
			desc = "Enable format-on-save",
		})
	end,
}
