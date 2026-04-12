vim.pack.add({
	-- Icons
	"https://github.com/nvim-mini/mini.icons",
	-- Tracking of git data and exposes :Git command
	"https://github.com/nvim-mini/mini-git",
	-- Integrates git diffs and hunks
	"https://github.com/nvim-mini/mini.diff",
	-- Status line
	"https://github.com/nvim-mini/mini.statusline",
	-- Extends forward/back functionality with brackets
	"https://github.com/nvim-mini/mini.bracketed",
	-- Add gS keybind to split/join arguments
	"https://github.com/nvim-mini/mini.splitjoin",
	-- Auto pairs
	"https://github.com/nvim-mini/mini.pairs",
	-- Surround functionality
	"https://github.com/nvim-mini/mini.surround",
	-- Extends a/i text objects
	"https://github.com/nvim-mini/mini.ai",
	-- Adds operators such as evaluating expressions, exchanging text, etc.
	"https://github.com/nvim-mini/mini.operators",
	-- File explorer
	"https://github.com/nvim-mini/mini.files",
	-- Fuzzy finder
	"https://github.com/nvim-mini/mini.pick",
	-- Extra functionality for mini.nvim plugins
	"https://github.com/nvim-mini/mini.extra",
})

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

require("mini.pick").setup({
	mappings = {
		move_down = '<C-j>',
		move_up   = '<C-k>',
	},
	options = {
		use_cache = true,
	},
	window = {
		config = function()
			local editor_height = vim.o.lines
			local editor_width = vim.o.columns

			local win_height = math.floor(0.75 * editor_height)
			local win_width = math.floor(0.5 * editor_width)

			return {
				anchor = 'NW',
				height = win_height,
				width = win_width,
				row = math.floor(0.45 * (editor_height - win_height)),
				col = math.floor(0.5 * (editor_width - win_width)),
			}
		end
	}
})

require("mini.extra").setup()

--- Keymaps

local kset = vim.keymap.set

local function get_visual_selection()
	vim.cmd('noau normal! "vy"')
	local text = vim.fn.getreg('v')
	vim.fn.setrg('v', {})
	return text
end

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

kset("n", "<C-p>", function()
	MiniExtra.pickers.git_files()
end, { desc = "Search project files" })

kset("n", "<leader>sc", function()
	MiniPick.builtin.files({ cwd = vim.fn.stdpath("config") })
end, { desc = "Search config files" })

kset("n", "<leader>sf", function()
	MiniPick.builtin.files()
end, { desc = "Search files" })

kset("n", "<leader>sb", function()
	MiniPick.builtin.buffers()
end, { desc = "Search buffers" })

kset("n", "<leader>sg", function()
	MiniPick.builtin.grep_live()
end, { desc = "Grep project" })

kset("n", "<leader>s:", function()
	MiniExtra.pickers.history({ scope = ':' })
end, { desc = "Search command history" })

kset("n", "<leader>sC", function()
	MiniExtra.pickers.commands()
end, { desc = "Search commands" })

kset("n", "<leader>sr", function()
	MiniExtra.pickers.oldfiles()
end, { desc = "Search recent files" })

kset("n", "<leader>sw", function()
	MiniPick.builtin.grep({ pattern = vim.fn.expand("<cword>") })
end, { desc = "Search word under cursor" })

kset("n", "<leader>sW", function()
	MiniPick.builtin.grep({ pattern = vim.fn.expand("<cWORD>") })
end, { desc = "Search WORD under cursor" })

kset({ "v" }, "<leader>sw", function()
	local text = get_visual_selection()
	MiniPick.builtin.grep({ pattern = text })
end, { desc = "Search visual selection" })

kset("n", '<leader>s"', function()
	MiniExtra.pickers.registers()
end, { desc = "Search registers" })

kset("n", "<leader>s/", function()
	MiniExtra.pickers.history({ scope = '/' })
end, { desc = "Search History" })

kset("n", "<leader>sd", function()
	MiniExtra.pickers.diagnostic({ scope = 'current' })
end, { desc = "Search document diagnostics" })

kset("n", "<leader>sD", function()
	MiniExtra.pickers.diagnostic({ scope = 'all' })
end, { desc = "Search workspace diagnostics" })

kset("n", "<leader>sh", function()
	MiniPick.builtin.help()
end, { desc = "Search help tags" })

kset("n", "<leader>sk", function()
	MiniExtra.pickers.keymaps()
end, { desc = "Search keymaps" })

kset("n", "<leader>sm", function()
	MiniExtra.pickers.marks()
end, { desc = "Search marks" })

kset("n", "<leader>sl", function()
	MiniExtra.pickers.list({ scope = 'location' })
end, { desc = "Search location list" })

kset("n", "<leader>sq", function()
	MiniExtra.pickers.list({ scope = 'quickfix' })
end, { desc = "Search quickfix list" })

kset("n", "<leader>st", function()
	MiniExtra.pickers.lsp({ scope = 'document_symbol' })
end, { desc = "Search LSP document symbols (tags)" })

kset("n", "<leader>sT", function()
	MiniExtra.pickers.lsp({ scope = 'workspace_symbol' })
end, { desc = "Search LSP workspace symbols (tags)" })

kset("n", "gd", function()
	MiniExtra.pickers.lsp({ scope = 'definition' })
end, { desc = "Search LSP definitions" })

kset("n", "gD", function()
	MiniExtra.pickers.lsp({ scope = 'declaration' })
end, { desc = "Search LSP declarations" })

kset("n", "gr", function()
	MiniExtra.pickers.lsp({ scope = 'references' })
end, {
	nowait = true,
	desc = "Search LSP references",
})

kset("n", "gI", function()
	MiniExtra.pickers.lsp({ scope = 'implementation' })
end, { desc = "Search LSP implementations" })

kset("n", "gy", function()
	MiniExtra.pickers.lsp({ scope = 'type_definition' })
end, { desc = "Search LSP t[y]pe definitions" })
