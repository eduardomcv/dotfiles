return {
	-- Add lua types for neovim
	{
		"folke/lazydev.nvim",
		ft = "lua", -- only load on lua files
		opts = {
			library = {
				-- Load luvit types when the `vim.uv` word is found
				{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
			},
		},
	},
	-- Completion
	{
		"saghen/blink.cmp",
		dependencies = {
			"rafamadriz/friendly-snippets",
			"lazydev.nvim",
		},
		version = "*",
		---@module 'blink.cmp'
		---@type blink.cmp.Config
		opts = {
			keymap = {
				preset = "enter",
				["<C-j>"] = { "select_next", "fallback" },
				["<C-k>"] = { "select_prev", "fallback" },
				cmdline = {
					preset = "super-tab",
					["<C-j>"] = { "select_next", "fallback" },
					["<C-k>"] = { "select_prev", "fallback" },
				},
			},
			appearance = {
				nerd_font_variant = "mono",
			},
			sources = {
				default = { "lazydev", "lsp", "path", "snippets", "buffer" },
				providers = {
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
	-- Treesitter parser integration
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		event = "VeryLazy",
		config = function()
			require("nvim-treesitter.configs").setup({
				auto_install = true,
				highlight = { enable = true },
				indent = { enable = true },
				ensure_installed = {
					"bash",
					"tmux",
					"diff",
					"lua",
					"vim",
					"regex",
					"markdown",
					"json",
					"jsonc",
					"yaml",
					"javascript",
					"typescript",
					"tsx",
					"jsdoc",
					"html",
					"css",
				},
			})
		end,
	},
	-- Auto pairs
	{
		"echasnovski/mini.pairs",
		version = "*",
		opts = {
			modes = {
				insert = true,
				command = true,
				terminal = false,
			},
		},
	},
	-- Auto tags using treesitter
	{
		"windwp/nvim-ts-autotag",
		deps = "nvim-treesitter",
	},
}
