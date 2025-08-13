return {
	-- Completion engine
	{
		"saghen/blink.cmp",
		dependencies = {
			-- Support snippets in completion menu
			{ "rafamadriz/friendly-snippets" },
			-- Better support for neovim config in lua
			{
				"folke/lazydev.nvim",
				ft = "lua", -- only load on lua files
				opts = {
					library = {
						-- Load luvit types when the `vim.uv` word is found
						{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
						{ path = "snacks.nvim", words = { "Snacks" } },
					},
				},
			},
			-- Copilot
			-- {
			-- 	"zbirenbaum/copilot.lua",
			-- 	cmd = "Copilot",
			-- 	event = "InsertEnter",
			-- 	opts = {
			-- 		suggestion = { enabled = false },
			-- 		panel = { enabled = false },
			-- 	},
			-- },
			-- Add copilot as completion source
			-- { "fang2hou/blink-copilot" },
			-- Compatibility layer for using nvim-cmp sources
			{
				"saghen/blink.compat",
				version = "*",
				lazy = true,
				opts = {},
			},
		},
		version = "*",
		---@module 'blink.cmp'
		---@type blink.cmp.Config
		opts = {
			appearance = {
				nerd_font_variant = "mono",
			},
			keymap = {
				preset = "enter",
				["<C-j>"] = { "select_next", "fallback" },
				["<C-k>"] = { "select_prev", "fallback" },
			},
			cmdline = {
				enabled = true,
				keymap = {
					preset = "cmdline",
					["<C-j>"] = { "select_next", "fallback" },
					["<C-k>"] = { "select_prev", "fallback" },
				},
			},
			sources = {
				default = {
					-- "copilot",
					"lazydev",
					"lsp",
					"path",
					"snippets",
					"buffer",
				},
				providers = {
					-- copilot = {
					-- 	name = "copilot",
					-- 	module = "blink-copilot",
					-- 	score_offset = 100,
					-- 	async = true,
					-- },
					lazydev = {
						name = "LazyDev",
						module = "lazydev.integrations.blink",
						score_offset = 100,
					},
				},
			},
		},
		opts_extend = { "sources.default" },
	},
}
