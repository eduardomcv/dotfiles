vim.pack.add({
	"https://github.com/nvim-flutter/flutter-tools.nvim",
	{ src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	"https://github.com/nvim-flutter/pubspec-assist.nvim",
})

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
			buf_set_keymap("n", "<leader>xi", ":FlutterInspectWidget<CR>", "Toggle Flutter widget inspector")
		end,
	},
})

require("pubspec-assist").setup()
