vim.pack.add({ "https://github.com/nvim-flutter/flutter-tools.nvim" })

require("flutter-tools").setup({
	flutter_lookup_cmd = "mise where flutter",
	ui = {
		notification_style = "native",
	},
	decorations = {
		statusline = {
			app_version = true,
			device = true,
		},
	},
	debugger = {
		enabled = true,
	},
	lsp = {
		on_attach = function(_, bufnr)
			local function buf_set_keymap(mode, lhs, rhs, desc)
				vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, silent = true, desc = desc })
			end

			buf_set_keymap("n", "<leader>fr", ":FlutterRun<CR>", "Run Flutter app")
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
