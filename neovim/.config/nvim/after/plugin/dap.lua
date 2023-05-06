local ok_dap, dap = pcall(require, 'dap')
if not ok_dap then return end

local ok_dapui, dapui = pcall(require, 'dapui')
if not ok_dapui then return end

local ok_dap_vscode_js, dap_vscode_js = pcall(require, 'dap-vscode-js')
if not ok_dap_vscode_js then return end

local utils = require('user.utils')
local mason_path = vim.fn.stdpath('data') .. '/mason'

dap_vscode_js.setup({
  adapters = { 'pwa-node' }, -- which adapters to register in nvim-dap
  debugger_path = mason_path .. '/packages/js-debug-adapter'
})

dap.adapters.firefox = {
  type = 'executable',
  command = 'node',
  args = { mason_path .. '/packages/firefox-debug-adapter/dist/adapter.bundle.js' },
}

dap.configurations.typescript = {
  {
    name = "Debug Jest tests",
    type = "pwa-node",
    request = "launch",
    trace = true, -- include debugger info
    runtimeExecutable = "node",
    runtimeArgs = {
      "./node_modules/jest/bin/jest.js",
      "--runInBand",
    },
    rootPath = "${workspaceFolder}",
    cwd = "${workspaceFolder}",
    console = "integratedTerminal",
    internalConsoleOptions = "neverOpen",
    sourceMaps = true,
  }
}

dap.configurations.typescriptreact = {
  {
    name = "Debug Jest tests",
    type = "pwa-node",
    request = "launch",
    trace = true, -- include debugger info
    runtimeExecutable = "node",
    runtimeArgs = {
      "./node_modules/jest/bin/jest.js",
      "--runInBand",
    },
    rootPath = "${workspaceFolder}",
    cwd = "${workspaceFolder}",
    console = "integratedTerminal",
    internalConsoleOptions = "neverOpen",
    sourceMaps = true,
  },
  {
    name = "Attach Firefox",
    type = "firefox",
    request = "attach",
  },
  {
    name = 'Launch Firefox',
    type = 'firefox',
    request = 'launch',
    reAttach = true,
    url = 'http://localhost:3000',
    webRoot = '${workspaceFolder}',
  },
}

dapui.setup()

-- We need to wait for execution to stop at the first breakpoint before showing the UI to give the source maps time to generate.
-- If we don't, the UI will close because the source maps haven't generated in time.
dap.listeners.after.event_breakpoint["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close {}
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close {}
end

-- Breakpoint signs
local sign = vim.fn.sign_define

sign("DapBreakpoint", { text = "●", texthl = "DapBreakpoint", linehl = "", numhl = "" })
sign("DapBreakpointCondition", { text = "●", texthl = "DapBreakpointCondition", linehl = "", numhl = "" })
sign("DapLogPoint", { text = "◆", texthl = "DapLogPoint", linehl = "", numhl = "" })

-- Keymaps
utils.nmap('gbp', dap.toggle_breakpoint)
utils.nmap('<F5>', dap.continue)
utils.nmap('<F10>', dap.step_over)
utils.nmap('<F11>', dap.step_into)
utils.nmap('<S-F11>', dap.step_out)
utils.nmap('<S-F5>', dap.terminate)
vim.keymap.set({ 'n', 'v' }, '<leader>db', dapui.eval)
