vim.pack.add({ "https://github.com/nvim-flutter/flutter-tools.nvim" })

require("flutter-tools").setup({
	flutter_lookup_cmd = "mise where flutter",
	ui = {
		border = "rounded",
		notification_style = "native",
	},
	debugger = {
		enabled = true,
		exception_breakpoints = {
			"unhandled",
		},
	},
	dev_log = {
		enabled = false,
		focus_on_open = false,
	},
	dev_tools = {
		autostart = true,
		auto_open_browser = true,
	},
	widget_guides = {
		enabled = false,
	},
	lsp = {
		settings = {
			showTodos = false,
			enableSnippets = true,
			completeFunctionCalls = true,
			updateImportsOnRename = true,
			renameFilesWithClasses = "prompt",
		},
		on_attach = function(_, bufnr)
			local function buf_set_keymap(mode, lhs, rhs, desc)
				vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, silent = true, desc = desc })
			end

			buf_set_keymap("n", "<leader>xx", ":FlutterRun<CR>", "Run Flutter app")
			buf_set_keymap("n", "<leader>xq", ":FlutterQuit<CR>", "Quit Flutter app")
			buf_set_keymap("n", "<leader>xr", ":FlutterReload<CR>", "Hot reload Flutter app")
			buf_set_keymap("n", "<leader>xR", ":FlutterRestart<CR>", "Hot restart Flutter app")
			buf_set_keymap("n", "<leader>xd", ":FlutterDevices<CR>", "List Flutter devices")
			buf_set_keymap("n", "<leader>xe", ":FlutterEmulators<CR>", "List Flutter emulators")
			buf_set_keymap("n", "<leader>xpg", ":FlutterPubGet<CR>", "Get Flutter dependencies")
			buf_set_keymap("n", "<leader>xpu", ":FlutterPubUpgrade<CR>", "Upgrade Flutter dependencies")
			buf_set_keymap("n", "<leader>xo", ":FlutterOutlineToggle<CR>", "Toggle Flutter outline")
			buf_set_keymap("n", "<leader>xD", ":FlutterDevTools<CR>", "Open Flutter DevTools")
			buf_set_keymap("n", "<leader>xo", ":FlutterOutlineToggle<CR>", "Toggle Flutter outline")
			buf_set_keymap("n", "<leader>xi", ":FlutterInspectWidget<CR>", "Toggle Flutter widget inspector")
		end,
	},
})

-- vim.api.nvim_create_autocmd("FileType", {
-- 	desc = "Defer attaching dartls to after flutter-tools is done",
-- 	pattern = "dart",
-- 	callback = function(args)
-- 		local bufnr = args.buf
--
-- 		vim.defer_fn(function()
-- 			if not vim.api.nvim_buf_is_valid(bufnr) then
-- 				return
-- 			end
--
-- 			local attached = vim.lsp.get_clients({ bufnr = bufnr, name = "dartls" })
--
-- 			if #attached == 0 then
-- 				local running = vim.lsp.get_clients({ name = "dartls" })
-- 				if #running > 0 then
-- 					vim.lsp.buf_attach_client(bufnr, running[1].id)
-- 				end
-- 			end
-- 		end, 250)
-- 	end,
-- })
--
-- local minifiles_prefix = "minifiles://"
-- local detach_minifiles_group = vim.api.nvim_create_augroup("DetachMiniFilesDartLS", { clear = true })
--
-- vim.api.nvim_create_autocmd("LspAttach", {
-- 	desc = "Detach dartls from mini.files buffers",
-- 	group = detach_minifiles_group,
-- 	callback = function(args)
-- 		local client = vim.lsp.get_client_by_id(args.data.client_id)
--
-- 		if client and client.name == "dartls" then
-- 			local bufname = vim.api.nvim_buf_get_name(args.buf)
--
-- 			if vim.startswith(bufname, minifiles_prefix) then
-- 				vim.lsp.buf_detach_client(args.buf, client.id)
-- 			end
-- 		end
-- 	end,
-- })
