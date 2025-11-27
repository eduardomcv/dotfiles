vim.pack.add({ "https://github.com/nvim-mini/mini.nvim" })

-- Icons
local mini_icons = require("mini.icons")
mini_icons.setup()
mini_icons.mock_nvim_web_devicons()

-- Tracking of git data and exposes :Git command
require("mini.git").setup()

-- Integrates git diffs and hunks
require("mini.diff").setup({
	view = {
		style = "sign",
	},
})

-- Status line
require("mini.statusline").setup()

-- Extends forward/back functionality with brackets
require("mini.bracketed").setup()

-- Add gS keybind to split/join arguments
require("mini.splitjoin").setup()

-- Auto pairs
require("mini.pairs").setup({
	modes = {
		insert = true,
		command = true,
		terminal = false,
	},
})

-- Surround functionality
require("mini.surround").setup()

-- Extends a/i text objects
require("mini.ai").setup()

-- Adds operators such as evaluating expressions, exchanging text, etc
require("mini.operators").setup()

-- File explorer
require("mini.files").setup({
	content = {
		-- Filter these files from the explorer
		filter = function(entry)
			return entry.name ~= ".DS_Store" and entry.name ~= ".git" and entry.name ~= ".direnv"
		end,
	},
})

--- Keymaps

vim.keymap.set("n", "<leader>e", function()
	local MiniFiles = require("mini.files")
	if not MiniFiles.close() then
		MiniFiles.open(vim.api.nvim_buf_get_name(0))
		MiniFiles.reveal_cwd()
	end
end, {
	desc = "Toggle file explorer (current buffer)",
})

vim.keymap.set("n", "<leader>r", function()
	local MiniFiles = require("mini.files")
	if not MiniFiles.close() then
		MiniFiles.open()
	end
end, {
	desc = "Toggle file explorer (root dir)",
})
