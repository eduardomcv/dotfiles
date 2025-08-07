-- plugins to load in vscode
local vscode_plugins = {
	-- Surround functionality
	{
		"echasnovski/mini.surround",
		version = "*",
		opts = {},
	},
}

-- plugins excluded from vscode
local other_plugins = {
	-- Completion
	{
		"saghen/blink.cmp",
		dependencies = {
			-- Support snippets in completion menu
			"rafamadriz/friendly-snippets",
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
			-- Add copilot as completion source
			"fang2hou/blink-copilot",
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
				keymap = {
					preset = "super-tab",
					["<C-j>"] = { "select_next", "fallback" },
					["<C-k>"] = { "select_prev", "fallback" },
				},
			},
			sources = {
				default = {
					"copilot",
					"lazydev",
					"lsp",
					"path",
					"snippets",
					"buffer",
				},
				providers = {
					copilot = {
						name = "copilot",
						module = "blink-copilot",
						score_offset = 100,
						async = true,
					},
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
			---@diagnostic disable-next-line: missing-fields
			require("nvim-treesitter.configs").setup({
				auto_install = true,
				highlight = { enable = true },
				indent = { enable = true },
				incremental_selection = {
					enable = true,
					keymaps = {
						init_selection = "<C-space>",
						node_incremental = "<C-space>",
						scope_incremental = false,
						node_decremental = "<bs>",
					},
				},
				textobjects = {
					move = {
						enable = true,
						goto_next_start = { ["]f"] = "@function.outer", ["]c"] = "@class.outer", ["]a"] = "@parameter.inner" },
						goto_next_end = { ["]F"] = "@function.outer", ["]C"] = "@class.outer", ["]A"] = "@parameter.inner" },
						goto_previous_start = { ["[f"] = "@function.outer", ["[c"] = "@class.outer", ["[a"] = "@parameter.inner" },
						goto_previous_end = { ["[F"] = "@function.outer", ["[C"] = "@class.outer", ["[A"] = "@parameter.inner" },
					},
				},
				ensure_installed = {
					"bash",
					"tmux",
					"diff",
					"lua",
					"vim",
					"regex",
					"markdown",
					"markdown_inline",
					"json",
					"jsonc",
					"yaml",
					"javascript",
					"typescript",
					"tsx",
					"jsdoc",
					"html",
					"css",
					"http",
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
		opts = {},
	},
	-- Enhances and extends a/i text objects
	{
		"echasnovski/mini.ai",
		version = "*",
		opts = {},
	},
	-- Copilot
	{
		"zbirenbaum/copilot.lua",
		cmd = "Copilot",
		event = "InsertEnter",
		opts = {
			suggestion = { enabled = false },
			panel = { enabled = false },
		},
	},
}

if IS_VSCODE then
	return vscode_plugins
end

return require("utils").table_join(vscode_plugins, other_plugins)
