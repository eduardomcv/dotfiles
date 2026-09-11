local use = require("core.use")

use({
	-- Show pop-up with available keybindings
	"folke/which-key.nvim",
	-- Improve tab integration
	"nanozuki/tabby.nvim",
	-- Highlight TODO comments
	{
		"folke/todo-comments.nvim",
		dependencies = {
			{ "nvim-lua/plenary.nvim", version = "master" },
		},
	},
	-- Highlight colors
	"brenoprata10/nvim-highlight-colors",
})

local is_using_kitty = os.getenv("TERM") == "xterm-kitty" or os.getenv("KITTY_PID") ~= nil

if is_using_kitty then
	-- Integrate navigation with kitty terminal
	use({ "knubie/vim-kitty-navigator" })
else
	-- Add C-h, C-j, C-k, C-l keymaps for window navigation
	vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Go to the left window" })
	vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Go to the down window" })
	vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Go to the up window" })
	vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Go to the right window" })
end

require("which-key").setup({
	spec = {
		{ "<leader>a", group = "+agents" },
		{ "<leader>c", group = "+code" },
		{ "<leader>f", group = "+format" },
		{ "<leader>g", group = "+git" },
		{ "<leader>n", group = "+notification" },
		{ "<leader>s", group = "+search" },
		{ "<leader>d", group = "+debug" },
		{ "<leader>t", group = "+tabs" },
		{ "<leader>x", group = "+context" },
		{ "<leader>u", group = "+test" },
	},
})

local todo_comments = require("todo-comments")
todo_comments.setup({})

local tabby = require("tabby")
tabby.setup()

require("nvim-highlight-colors").setup({})

--- Keymaps

local function rename_tab()
	local tab_name = vim.fn.input({ prompt = "New tab name: " })
	if tab_name ~= "" then
		tabby.tab_rename(tab_name)
	end
end

vim.keymap.set("n", "<leader>tr", rename_tab, { desc = "Rename tab" })

vim.keymap.set("n", "]k", function()
	todo_comments.jump_next()
end, { desc = "Next TODO comment" })

vim.keymap.set("n", "[k", function()
	todo_comments.jump_prev()
end, { desc = "Previous TODO comment" })
