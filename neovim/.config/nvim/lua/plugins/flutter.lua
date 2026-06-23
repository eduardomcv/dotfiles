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
})
