vim.pack.add({
	{ src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	"https://github.com/esmuellert/codediff.nvim",
	"https://github.com/m00qek/baleia.nvim",
	-- Magit for neovim
	"https://github.com/neogitorg/neogit",
})

vim.keymap.set("n", "<leader>gg", "<cmd>Neogit<cr>", { desc = "Show Neogit UI" })
