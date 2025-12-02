vim.pack.add({
	{ src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
	"https://github.com/olimorris/codecompanion.nvim",
})

require("codecompanion").setup({})
