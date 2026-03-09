vim.pack.add({
	{ src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
	{ src = "https://github.com/olimorris/codecompanion.nvim", version = vim.version.range("^17.33.0") },
})

require("codecompanion").setup({
	ignore_warnings = true,
	display = {
		action_palette = {
			provider = "snacks",
		},
	},
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
})

--- Keymaps
vim.keymap.set({ "n", "v" }, "<leader>aa", "<cmd>CodeCompanionActions<cr>")
vim.keymap.set({ "n", "v" }, "<leader>ac", "<cmd>CodeCompanionChat Toggle<cr>")
vim.keymap.set("v", "ga", "<cmd>CodeCompanionChat Add<cr>")
