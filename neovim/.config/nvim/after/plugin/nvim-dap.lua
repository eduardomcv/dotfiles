local status, dap = pcall(require, 'dap')
if not status then return end

local set = vim.keymap.set

set('n', '<leader>bp', dap.toggle_breakpoint)
set('n', '<leader>si', dap.step_into)
set('n', '<leader>so', dap.step_over)
set('n', '<leader>sb', dap.step_out)
set('n', '<leader>ss', dap.continue)

vim.fn.sign_define('DapBreakpoint', { text = '🔴', texthl = '', linehl = '', numhl = '' })
vim.fn.sign_define('DapStopped', { text = '🟩', texthl = '', linehl = '', numhl = '' })

dap.adapters.chrome = {
  type = "executable",
  command = "node",
  args = { os.getenv("HOME") .. "/.local/share/nvim/mason/packages/chrome-debug-adapter/out/src/chromeDebug.js" }
}

dap.configurations.typescriptreact = {
  {
    type = "chrome",
    request = "attach",
    program = "${file}",
    cwd = vim.fn.getcwd(),
    sourceMaps = true,
    protocol = "inspector",
    port = 9222,
    webRoot = "${workspaceFolder}"
  },
}
