vim.pack.add({
	{ src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	-- Show pop-up with available keybindings
	"https://github.com/folke/which-key.nvim",
	-- Improve tab integration
	"https://github.com/nanozuki/tabby.nvim",
	-- Highlight TODO comments
	"https://github.com/folke/todo-comments.nvim",
	-- Highlight colors
	"https://github.com/brenoprata10/nvim-highlight-colors",
	-- Integrate with kitty terminal
	"https://github.com/knubie/vim-kitty-navigator",
})

require("which-key").setup({
	spec = {
		{ "<leader>a", group = "+agent" },
		{ "<leader>c", group = "+code" },
		{ "<leader>f", group = "+format" },
		{ "<leader>g", group = "+git" },
		{ "<leader>n", group = "+notification" },
		{ "<leader>s", group = "+search" },
		{ "<leader>o", group = "+opencode" },
	},
})

require("todo-comments").setup({})

require("tabby").setup()

require("nvim-highlight-colors").setup({})

--- Keymaps

local function rename_tab()
	local tab_name = vim.fn.input({ prompt = "New tab name" })
	if tab_name ~= "" then
		require("tabby").tab_rename(tab_name)
	end
end

vim.keymap.set("n", "<leader>tr", rename_tab, { desc = "Rename tab" })
