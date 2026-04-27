vim.pack.add({
	{ src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
	{ src = "https://github.com/hakonharnes/img-clip.nvim" },
	{ src = "https://github.com/MeanderingProgrammer/render-markdown.nvim" },
	{ src = "https://github.com/ravitemer/codecompanion-history.nvim" },
	{ src = "https://github.com/olimorris/codecompanion.nvim" },
})

require("img-clip").setup()

require("render-markdown").setup({
	preset = "lazy",
})

require("codecompanion").setup({
	adapters = {
		acp = {
			gemini_cli = function()
				return require("codecompanion.adapters").extend("gemini_cli", {
					defaults = {
						auth_method = "oauth-personal",
					},
				})
			end,
		},
	},
	display = {
		action_palette = {
			provider = "snacks",
		},
		chat = {
			window = {
				position = "right",
				width = 0.3,
			},
		},
	},
	extensions = {
		history = {
			opts = {
				picker = "snacks",
				title_generation_opts = {
					adapter = "copilot",
					model = "gpt-4o",
				},
			},
		},
	},
	interactions = {
		chat = {
			adapter = {
				name = "opencode",
				model = "github-copilot/claude-sonnet-4.6",
			},
			opts = {
				completion_provider = "blink",
			},
		},
	},
})

--- Keymaps
vim.keymap.set({ "n" }, "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste image from clipboard" })
vim.keymap.set({ "n", "v" }, "<leader>aa", "<cmd>CodeCompanionActions<cr>", { desc = "Show CodeCompanion actions" })
vim.keymap.set({ "n", "v" }, "<leader>ac", "<cmd>CodeCompanionChat Toggle<cr>", { desc = "Toggle CodeCompanion chat" })
vim.keymap.set("v", "ga", "<cmd>CodeCompanionChat Add<cr>", { desc = "Add selection to CodeCompanion chat" })
