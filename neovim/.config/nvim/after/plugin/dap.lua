local ok_dap, dap = pcall(require, 'dap')
if not ok_dap then return end

local ok_dapui, dapui = pcall(require, 'dapui')
if not ok_dapui then return end

local utils = require('eduardomcv.utils')
local mason_path = vim.fn.stdpath('data') .. '/mason'

dap.adapters.firefox = {
  type = 'executable',
  command = 'node',
  args = { mason_path .. '/packages/firefox-debug-adapter/dist/adapter.bundle.js' },
}

dap.adapters.node2 = {
  type = 'executable',
  command = 'node',
  args = { mason_path .. '/packages/node-debug2-adapter/out/src/nodeDebug.js' },
}

dap.configurations.typescriptreact = {
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

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open {}
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close {}
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close {}
end

-- Breakpoint signs
vim.fn.sign_define('DapBreakpoint', { text = '🔴', texthl = '', linehl = '', numhl = '' })
vim.fn.sign_define('DapStopped', { text = '🟩', texthl = '', linehl = '', numhl = '' })

-- Keymaps
utils.nmap('gbp', dap.toggle_breakpoint)
utils.nmap('gsi', dap.step_into)
utils.nmap('gso', dap.step_over)
utils.nmap('gsb', dap.step_out)
utils.nmap('gss', dap.continue)
utils.nmap('gst', dap.terminate)
vim.keymap.set({ 'n', 'v' }, 'gee', dapui.eval)
