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
		open_cmd = "tabnew",
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
			enableSnippets = false,
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
