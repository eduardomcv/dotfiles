vim.pack.add({ "https://github.com/mfussenegger/nvim-lint" })

local lint = require("lint")

lint.linters_by_ft = {}

--[[
Events to trigger linting:
	- After writing the buffer
	- After reading the file into the buffer
	- When leaving insert mode
]]
local events = { "BufWritePost", "BufReadPost", "InsertLeave" }

-- Create autocmd for linting triggers
vim.api.nvim_create_autocmd(events, {
	group = vim.api.nvim_create_augroup("nvim-lint", { clear = true }),
	callback = function()
		lint.try_lint()
	end,
})
