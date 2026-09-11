local use = require("core.use")

use({
	{
		"nvim-neotest/neotest",
		dependencies = {
			{ "nvim-lua/plenary.nvim", version = "master" },
			"antoinemadec/FixCursorHold.nvim",
		},
	},
	"marilari88/neotest-vitest",
	"sidlatau/neotest-dart",
	"nvim-neotest/neotest-python",
	{
		"mrcjkb/rustaceanvim",
		version = vim.version.range("^9"),
	},
})

local neotest = require("neotest")

---@diagnostic disable-next-line: missing-fields
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
		require("rustaceanvim.neotest"),
	},
})

vim.keymap.set("n", "<leader>ut", function()
	neotest.run.run()
end, { desc = "Test: Run nearest" })

vim.keymap.set("n", "<leader>ud", function()
	---@diagnostic disable-next-line: missing-fields
	neotest.run.run({ strategy = "dap" })
end, { desc = "Test: Debug nearest" })

vim.keymap.set("n", "<leader>uf", function()
	neotest.run.run(vim.fn.expand("%"))
end, { desc = "Test: Run file" })

vim.keymap.set("n", "<leader>us", neotest.run.stop, { desc = "Test: Stop" })

vim.keymap.set("n", "<leader>uw", function()
	neotest.watch.toggle()
end, { desc = "Test: Toggle watch" })

vim.keymap.set("n", "<leader>uo", neotest.output_panel.toggle, { desc = "Test: Toggle output panel" })

vim.keymap.set("n", "<leader>uS", neotest.summary.toggle, { desc = "Test: Toggle summary" })
