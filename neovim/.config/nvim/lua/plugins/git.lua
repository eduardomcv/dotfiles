local use = require("core.use")

use({
	"esmuellert/codediff.nvim",
	{
		-- Magit for neovim
		"neogitorg/neogit",
		dependencies = {
			{ "nvim-lua/plenary.nvim", version = "master" },
			"m00qek/baleia.nvim",
		},
	},
	{
		-- Git conflicts visualizer
		"akinsho/git-conflict.nvim",
		version = vim.version.range("*"),
	},
})

---@diagnostic disable-next-line: missing-fields
require("git-conflict").setup({})

vim.keymap.set("n", "<leader>gg", "<cmd>Neogit<cr>", { desc = "Show Neogit UI" })
