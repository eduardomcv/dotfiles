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
})

local is_using_kitty = os.getenv("TERM") == "xterm-kitty" or os.getenv("KITTY_PID") ~= nil

if is_using_kitty then
	-- Integrate navigation with kitty terminal
	vim.pack.add({ "https://github.com/knubie/vim-kitty-navigator" })
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

require("todo-comments").setup({})

require("tabby").setup()

require("nvim-highlight-colors").setup({})

--- Keymaps

local function rename_tab()
	local tab_name = vim.fn.input({ prompt = "New tab name: " })
	if tab_name ~= "" then
		require("tabby").tab_rename(tab_name)
	end
end

vim.keymap.set("n", "<leader>tr", rename_tab, { desc = "Rename tab" })
