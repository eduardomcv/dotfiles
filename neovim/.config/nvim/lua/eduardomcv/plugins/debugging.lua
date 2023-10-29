-- Debugging plugins

return {
  "rcarriga/nvim-dap-ui",
  enabled = false, -- WIP: disabled for now. Will revisit when needed.
  dependencies = {
    "mfussenegger/nvim-dap",
    "mxsdev/nvim-dap-vscode-js",
  },
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local dap, dapui = require("dap"), require("dapui")
    local u = require("eduardomcv.utils")

    local mason_path = vim.fn.stdpath("data") .. "/mason"

    require("dap-vscode-js").setup({
      adapters = { "pwa-node" }, -- which adapters to register in nvim-dap
      debugger_path = mason_path .. "/packages/js-debug-adapter",
    })

    dap.adapters.firefox = {
      type = "executable",
      command = "node",
      args = { mason_path .. "/packages/firefox-debug-adapter/dist/adapter.bundle.js" },
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
      },
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
        name = "Launch Firefox",
        type = "firefox",
        request = "launch",
        reAttach = true,
        url = "http://localhost:3000",
        webRoot = "${workspaceFolder}",
      },
    }

    dapui.setup()

    -- We need to wait for execution to stop at the first breakpoint before showing the UI to give the source maps time to generate.
    -- If we don't, the UI will close because the source maps haven't generated in time.
    dap.listeners.after.event_breakpoint["dapui_config"] = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated["dapui_config"] = function()
      dapui.close()
    end
    dap.listeners.before.event_exited["dapui_config"] = function()
      dapui.close()
    end

    -- Breakpoint signs
    local sign = vim.fn.sign_define

    sign("DapBreakpoint", { text = "●", texthl = "DapBreakpoint", linehl = "", numhl = "" })
    sign("DapBreakpointCondition", { text = "●", texthl = "DapBreakpointCondition", linehl = "", numhl = "" })
    sign("DapLogPoint", { text = "◆", texthl = "DapLogPoint", linehl = "", numhl = "" })

    -- Keymaps
    u.nmap("gbp", dap.toggle_breakpoint)
    u.nmap("<F5>", dap.continue)
    u.nmap("<F10>", dap.step_over)
    u.nmap("<F11>", dap.step_into)
    u.nmap("<S-F11>", dap.step_out)
    u.nmap("<S-F5>", dap.terminate)
    vim.keymap.set({ "n", "v" }, "<leader>db", dapui.eval)
  end,
}
