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
		filter = function(entry)
			-- Filter these files from the explorer
			local hidden_patterns = {
				"%.DS_Store",
				"%.git$",
				"%.direnv",
				"%.pytest_cache",
				"%.ruff_cache",
				"%.venv",
				"__pycache__",
				"%.egg%-info$",
			}

			for _, pattern in ipairs(hidden_patterns) do
				if entry.name:find(pattern) then
					return false
				end
			end

			return true
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
