vim.pack.add({
	{ src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	"https://github.com/antoinemadec/FixCursorHold.nvim",
	"https://github.com/nvim-neotest/neotest",
	"https://github.com/marilari88/neotest-vitest",
	"https://github.com/sidlatau/neotest-dart",
	"https://github.com/nvim-neotest/neotest-python",
})

local neotest = require("neotest")

neotest.setup({
	adapters = {
		require("neotest-vitest"),
		require("neotest-python")({
			dap = { justMyCode = false },
		}),
		require("neotest-dart")({
			command = "flutter",
			use_lsp = true,
		}),
	},
})

vim.keymap.set("n", "<leader>tt", function()
	neotest.run.run()
end, { desc = "Test: Run nearest" })

vim.keymap.set("n", "<leader>td", function()
	neotest.run.run({ strategy = "dap" })
end, { desc = "Test: Debug nearest" })

vim.keymap.set("n", "<leader>tf", function()
	neotest.run.run(vim.fn.expand("%"))
end, { desc = "Test: Run file" })

vim.keymap.set("n", "<leader>ts", neotest.run.stop, { desc = "Test: Stop" })

vim.keymap.set("n", "<leader>tw", function()
	neotest.watch.toggle()
end, { desc = "Test: Toggle watch" })

vim.keymap.set("n", "<leader>to", neotest.output_panel.toggle, { desc = "Test: Toggle output panel" })

vim.keymap.set("n", "<leader>tS", neotest.summary.toggle, { desc = "Test: Toggle summary" })
