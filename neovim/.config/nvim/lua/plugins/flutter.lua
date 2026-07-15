vim.pack.add({ "https://github.com/nvim-flutter/flutter-tools.nvim" })

require("flutter-tools").setup({
	flutter_lookup_cmd = "mise where flutter",
	ui = {
		border = "rounded",
		notification_style = "native",
	},
	decorations = {
		statusline = {
			device = true,
			app_version = true,
			project_config = false,
		},
	},
	dev_log = {
		enabled = true,
		focus_on_open = false,
	},
	debugger = {
		enabled = true,
	},
	widget_guides = {
		enabled = false,
	},
	lsp = {
		settings = {
			showTodos = false,
			enableSnippets = true,
			completeFunctionCalls = true,
			renameFilesWithClasses = "prompt",
		},
		on_attach = function(_, bufnr)
			local function buf_set_keymap(mode, lhs, rhs, desc)
				vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, silent = true, desc = desc })
			end

			buf_set_keymap("n", "<leader>fx", ":FlutterRun<CR>", "Run Flutter app")
			buf_set_keymap("n", "<leader>fq", ":FlutterQuit<CR>", "Quit Flutter app")
			buf_set_keymap("n", "<leader>fr", ":FlutterReload<CR>", "Hot reload Flutter app")
			buf_set_keymap("n", "<leader>fR", ":FlutterRestart<CR>", "Hot restart Flutter app")
			buf_set_keymap("n", "<leader>fd", ":FlutterDevices<CR>", "List Flutter devices")
			buf_set_keymap("n", "<leader>fe", ":FlutterEmulators<CR>", "List Flutter emulators")
			buf_set_keymap("n", "<leader>fpg", ":FlutterPubGet<CR>", "Get Flutter dependencies")
			buf_set_keymap("n", "<leader>fpu", ":FlutterPubUpgrade<CR>", "Upgrade Flutter dependencies")
			buf_set_keymap("n", "<leader>fo", ":FlutterOutlineToggle<CR>", "Toggle Flutter outline")
			buf_set_keymap("n", "<leader>fD", ":FlutterDevTools<CR>", "Open Flutter DevTools")
		end,
	},
})

local flutter_dev_log_buf_name = "__FLUTTER_DEV_LOG__"
local flutter_log_user_opened = false

local function toggle_flutter_dev_log()
	local log_buf = nil
	for _, buf in ipairs(vim.api.nvim_list_bufs()) do
		local name = vim.api.nvim_buf_get_name(buf)
		if name:match(flutter_dev_log_buf_name) then
			log_buf = buf
			break
		end
	end

	if not log_buf then
		vim.notify("Flutter dev log buffer not available!", vim.log.levels.INFO)
		return
	end

	local wins = vim.fn.win_findbuf(log_buf)
	if #wins > 0 then
		flutter_log_user_opened = false
		for _, win in ipairs(wins) do
			vim.api.nvim_win_close(win, false)
		end
	else
		flutter_log_user_opened = true
		vim.cmd("botright vsplit")
		vim.api.nvim_win_set_buf(0, log_buf)
	end
end

vim.keymap.set("n", "<leader>fl", toggle_flutter_dev_log, { desc = "Toggle Flutter dev log" })

vim.api.nvim_create_autocmd("BufWinEnter", {
	pattern = "*" .. flutter_dev_log_buf_name .. "*",
	callback = function(ev)
		if flutter_log_user_opened then
			return
		end
		vim.defer_fn(function()
			local wins = vim.fn.win_findbuf(ev.buf)
			for _, win in ipairs(wins) do
				pcall(vim.api.nvim_win_close, win, false)
			end
		end, 1)
	end,
	desc = "Close flutter log buffer when opening a new window",
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "dart",
	callback = function(args)
		local bufnr = args.buf

		-- Defer slightly to ensure flutter-tools finishes its own logic first
		vim.defer_fn(function()
			if not vim.api.nvim_buf_is_valid(bufnr) then
				return
			end

			-- Check if dartls is attached
			local attached = vim.lsp.get_clients({ bufnr = bufnr, name = "dartls" })

			if #attached == 0 then
				-- Find the orphaned dartls instance running in the background
				local running = vim.lsp.get_clients({ name = "dartls" })
				if #running > 0 then
					-- Force the attachment
					vim.lsp.buf_attach_client(bufnr, running[1].id)
				end
			end
		end, 250)
	end,
	desc = "Bridge flutter-tools to Neovim 0.12 LSP auto-attach",
})
