vim.pack.add({ "https://github.com/mfussenegger/nvim-lint" })

local linter_name = "bloc"
local pattern = "(%w+)%[([%w_]+)%]:%s*(.-)\n%s*%-%->%s*.-:(%d+)\n%s*|\n%s*|.-\n%s*| (%s*)(%^+)"
local severity_map = {
	error = vim.diagnostic.severity.ERROR,
	warning = vim.diagnostic.severity.WARN,
	info = vim.diagnostic.severity.INFO,
}

---@type lint.parse
local function parse_bloc_output(output, bufnr)
	local diagnostics = {}

	for sev_str, code, msg, line_number, spaces, carets in output:gmatch(pattern) do
		local col = #spaces
		local end_col = col + #carets
		local lnum = tonumber(line_number) - 1
		local severity = severity_map[sev_str] or vim.diagnostic.severity.WARN

		table.insert(diagnostics, {
			bufnr = bufnr,
			source = linter_name,
			code = code,
			message = msg,
			severity = severity,
			lnum = lnum,
			col = col,
			end_col = end_col,
		})
	end

	return diagnostics
end

require("lint").linters[linter_name] = {
	name = linter_name,
	cmd = "bloc",
	args = { "lint" },
	stdin = false,
	append_fname = true,
	stream = "stdout",
	ignore_exitcode = true,
	parser = parse_bloc_output,
}

require("lint").linters_by_ft = {
	python = { "ruff" },
	bash = { "shellcheck" },
	lua = { "luacheck" },
	dart = { "bloc" },
}

--[[
Events to trigger linting:
	- After writing the buffer
	- When entering a buffer window
	- When leaving insert mode
]]
local lint_events = {
	"BufWritePost",
	"BufWinEnter",
	"InsertLeave",
}

-- Create autocmd for linting triggers
local autocmd_group = vim.api.nvim_create_augroup("nvim-lint", { clear = true })
vim.api.nvim_create_autocmd(lint_events, {
	group = autocmd_group,
	callback = function()
		require("lint").try_lint()
	end,
})
