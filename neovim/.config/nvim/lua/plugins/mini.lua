vim.pack.add({ "https://github.com/nvim-mini/mini.nvim" })

local mini_icons = require("mini.icons")
mini_icons.setup()
mini_icons.mock_nvim_web_devicons()

require("mini.git").setup()

require("mini.diff").setup({
	view = {
		style = "sign",
	},
})

require("mini.statusline").setup()

require("mini.bracketed").setup()

require("mini.splitjoin").setup()

require("mini.pairs").setup({
	modes = {
		insert = true,
		command = true,
		terminal = false,
	},
})

require("mini.surround").setup()

require("mini.ai").setup()

require("mini.operators").setup()

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

local kset = vim.keymap.set

kset("n", "<leader>e", function()
	local MiniFiles = require("mini.files")
	if not MiniFiles.close() then
		MiniFiles.open(vim.api.nvim_buf_get_name(0))
		MiniFiles.reveal_cwd()
	end
end, {
	desc = "Toggle file explorer (current buffer)",
})

kset("n", "<leader>r", function()
	local MiniFiles = require("mini.files")
	if not MiniFiles.close() then
		MiniFiles.open()
	end
end, {
	desc = "Toggle file explorer (root dir)",
})
