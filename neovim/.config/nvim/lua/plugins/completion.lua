vim.pack.add({
	-- Support snippets in completion menu
	"https://github.com/rafamadriz/friendly-snippets",
	-- Better support for neovim lua
	"https://github.com/folke/lazydev.nvim",
	{
		-- Completion engine
		src = "https://github.com/saghen/blink.cmp",
		version = vim.version.range("1.*"),
	},
	{
		-- Compatibility layer for using nvim-cmp sources
		src = "https://github.com/saghen/blink.compat",
		version = vim.version.range("*"),
	},
})

require("lazydev").setup({
	library = {
		-- Load luvit types when the `vim.uv` word is found
		{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
		{ path = "snacks.nvim", words = { "Snacks" } },
	},
})

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
	completion = {
		menu = {
			draw = {
				components = {
					-- customize the drawing of kind icons
					kind_icon = {
						text = function(ctx)
							-- default kind icon
							local icon = ctx.kind_icon
							-- if LSP source, check for color derived from documentation
							if ctx.item.source_name == "LSP" then
								local color_item = require("nvim-highlight-colors").format(ctx.item.documentation, { kind = ctx.kind })
								if color_item and color_item.abbr ~= "" then
									icon = color_item.abbr
								end
							end
							return icon .. ctx.icon_gap
						end,
						highlight = function(ctx)
							-- default highlight group
							local highlight = "BlinkCmpKind" .. ctx.kind
							-- if LSP source, check for color derived from documentation
							if ctx.item.source_name == "LSP" then
								local color_item = require("nvim-highlight-colors").format(ctx.item.documentation, { kind = ctx.kind })
								if color_item and color_item.abbr_hl_group then
									highlight = color_item.abbr_hl_group
								end
							end
							return highlight
						end,
					},
				},
			},
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
