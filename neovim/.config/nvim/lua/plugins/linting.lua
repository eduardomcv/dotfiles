vim.pack.add({ "https://github.com/mfussenegger/nvim-lint" })

local bloc_lint_name = "bloc_lint"

local function bloc_parser(output)
	local diagnostics = {}
	local current_diagostic = nil

	local output_lines = vim.split(output, "\n")

	for _, line in ipairs(output_lines) do
		-- bloc lint output format: <severity>[<code>]: <message>
		-- Example:
		--   warning[avoid_flutter_imports]: Avoid importing Flutter within bloc instances.
		local sev_str, code, msg = line:match("^(%w+)%[([%w_]+)%]:%s*(.*)")

		if sev_str and code and msg then
			local severity = vim.diagnostic.severity.INFO

			if sev_str == "warning" then
				severity = vim.diagnostic.severity.WARN
			end

			if sev_str == "error" then
				severity = vim.diagnostic.severity.ERROR
			end

			current_diagostic = {
				source = bloc_lint_name,
				code = code,
				message = msg,
				severity = severity,
			}
		end

		if current_diagostic then
			-- The line immediately below the lint problem contains the file name and line number,
			-- in the format: ---> <file>:<line_number>.
			-- Example:
			--   ---> lib/bloc/counter/counter_bloc.dart:10
			local file, lnum = line:match("%-%->%s*(.-):(%d+)")

			if file and lnum then
				current_diagostic.lnum = tonumber(lnum) - 1
				current_diagostic.col = 0

				table.insert(diagnostics, current_diagostic)
				-- Reset current_diagnostic after adding it to the diagostics table.
				current_diagostic = nil
			end
		end
	end

	return diagnostics
end

require("lint").linters[bloc_lint_name] = {
	name = bloc_lint_name,
	cmd = "bloc",
	args = { "lint" },
	stdin = false,
	append_fname = true,
	stream = "stdout",
	ignore_exitcode = true,
	parser = bloc_parser,
}

require("lint").linters_by_ft = {
	python = { "ruff" },
	bash = { "shellcheck" },
	dart = { "bloc_lint" },
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
