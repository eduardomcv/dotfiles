vim.pack.add({
	-- Support snippets in completion menu
	"https://github.com/rafamadriz/friendly-snippets",
	-- Better support for neovim lua
	"https://github.com/folke/lazydev.nvim",
	{
		-- Completion engine
		src = "https://github.com/saghen/blink.cmp",
		version = "1.*",
	},
	{
		-- Compatibility layer for using nvim-cmp sources
		src = "https://github.com/saghen/blink.compat",
		version = "*",
	},
	-- Copilot
	-- "https://github.com/zbirenbaum/copilot.lua",
	-- Add copilot as completion source
	-- "fang2hou/blink-copilot",
})

require("lazydev").setup({
	library = {
		-- Load luvit types when the `vim.uv` word is found
		{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
		{ path = "snacks.nvim", words = { "Snacks" } },
	},
})

-- require("copilot").setup({
-- 	suggestion = { enabled = false },
-- 	panel = { enabled = false },
-- })

require("blink-compat").setup({})

require("blink-cmp").setup({
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
})
