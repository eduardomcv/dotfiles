vim.pack.add({
	"https://github.com/mfussenegger/nvim-dap",
	"https://github.com/nvim-neotest/nvim-nio",
	"https://github.com/rcarriga/nvim-dap-ui",
	"https://github.com/theHamsta/nvim-dap-virtual-text",
})

local dap = require("dap")
local dapui = require("dapui")

---@diagnostic disable: missing-fields
dapui.setup({
	controls = {
		enabled = true,
		element = "repl",
	},
	layouts = {
		{
			elements = {
				{ id = "scopes", size = 0.33 },
				{ id = "breakpoints", size = 0.17 },
				{ id = "stacks", size = 0.25 },
				{ id = "watches", size = 0.25 },
			},
			position = "left",
			size = 45,
		},
		{
			elements = {
				{ id = "repl", size = 0.5 },
				{ id = "console", size = 0.5 },
			},
			position = "bottom",
			size = 12,
		},
	},
})
---@diagnostic enable: missing-fields

require("nvim-dap-virtual-text").setup({})

local sign = vim.fn.sign_define

sign("DapBreakpoint", { text = " ", texthl = "DapBreakpoint", linehl = "", numhl = "DapBreakpoint" })
sign(
	"DapBreakpointCondition",
	{ text = " ", texthl = "DapBreakpointCondition", linehl = "", numhl = "DapBreakpointCondition" }
)
sign("DapBreakpointRejected", { text = " ", texthl = "DapBreakpoint", linehl = "", numhl = "DapBreakpoint" })
sign("DapLogPoint", { text = " ", texthl = "DapLogPoint", linehl = "", numhl = "DapLogPoint" })
sign("DapStopped", { text = " ", texthl = "DapLogPoint", linehl = "Visual", numhl = "DapLogPoint" })

dap.listeners.before.attach.dapui_config = function()
	dapui.open()
end

dap.listeners.before.launch.dapui_config = function()
	dapui.open()
end

dap.listeners.before.event_terminated.dapui_config = function()
	dapui.close()
end

dap.listeners.before.event_exited.dapui_config = function()
	dapui.close()
end

-- This needs to be installed via Mason
local js_debug_server = vim.fn.stdpath("data") .. "/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js"

dap.adapters["pwa-node"] = {
	type = "server",
	host = "127.0.0.1",
	port = "${port}",
	executable = {
		command = "node",
		args = { js_debug_server, "${port}", "127.0.0.1" },
	},
}

-- Alias so existing .vscode/launch.json files using type "node" also work
dap.adapters["node"] = function(callback, config)
	if config.type == "node" then
		config.type = "pwa-node"
	end

	local adapter = dap.adapters["pwa-node"]

	if type(adapter) == "function" then
		adapter(callback, config)
	else
		callback(adapter)
	end
end

dap.adapters.python = {
	type = "executable",
	command = vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python",
	args = { "-m", "debugpy.adapter" },
}

for _, ft in ipairs({ "typescript", "typescriptreact", "javascript", "javascriptreact" }) do
	dap.configurations[ft] = {
		{
			type = "pwa-node",
			request = "launch",
			name = "Launch file",
			program = "${file}",
			cwd = "${workspaceFolder}",
			sourceMaps = true,
			skipFiles = { "<node_internals>/**" },
		},
		{
			type = "pwa-node",
			request = "attach",
			name = "Attach to process",
			processId = require("dap.utils").pick_process,
			cwd = "${workspaceFolder}",
		},
	}
end

vim.keymap.set("n", "<leader>dc", dap.continue, { desc = "DAP: Continue" })
vim.keymap.set("n", "<leader>ds", dap.step_over, { desc = "DAP: Step over" })
vim.keymap.set("n", "<leader>di", dap.step_into, { desc = "DAP: Step into" })
vim.keymap.set("n", "<leader>do", dap.step_out, { desc = "DAP: Step out" })
vim.keymap.set("n", "<leader>dr", dap.restart, { desc = "DAP: Restart" })
vim.keymap.set("n", "<leader>dt", dap.terminate, { desc = "DAP: Terminate" })
vim.keymap.set("n", "<leader>dd", dap.disconnect, { desc = "DAP: Disconnect" })
vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint, { desc = "DAP: Toggle breakpoint" })
vim.keymap.set("n", "<leader>dR", dap.repl.toggle, { desc = "DAP: Toggle REPL" })

vim.keymap.set("n", "<leader>dB", function()
	dap.set_breakpoint(vim.fn.input("Breakpoint condition: "))
end, { desc = "DAP: Conditional breakpoint" })

vim.keymap.set("n", "<leader>dl", function()
	dap.set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
end, { desc = "DAP: Log point" })

vim.keymap.set("n", "<leader>du", function()
	dapui.toggle()
end, { desc = "DAP: Toggle UI" })

vim.keymap.set({ "n", "v" }, "<leader>dh", function()
	dapui.eval()
end, { desc = "DAP: Evaluate word/selection" })
