local vscode = require("vscode")

local kset = vim.keymap.set

--- Call a VSCode action asynchronously (thin wrapper).
---@param name string VSCode command id
---@param opts? table vscode.action opts
local function action(name, opts)
	return function()
		vscode.action(name, opts)
	end
end

-- ──────────────────────────────────────────────────────────────────────────────
-- Tab management
-- ──────────────────────────────────────────────────────────────────────────────

kset("n", "<leader>tn", action("workbench.action.addRootFolder"), { desc = "New editor group tab" })
kset("n", "<leader>tc", action("workbench.action.closeActiveEditor"), { desc = "Close active editor" })
kset("n", "gt", action("workbench.action.nextEditor"), { desc = "Next editor tab" })
kset("n", "gT", action("workbench.action.previousEditor"), { desc = "Previous editor tab" })

-- ──────────────────────────────────────────────────────────────────────────────
-- Explorer
-- ──────────────────────────────────────────────────────────────────────────────

kset("n", "<leader>e", action("workbench.files.action.showActiveFileInExplorer"), {
	desc = "Reveal current file in explorer",
})

-- ──────────────────────────────────────────────────────────────────────────────
-- Search / picker
-- ──────────────────────────────────────────────────────────────────────────────

kset("n", "<C-p>", action("workbench.action.quickOpen"), { desc = "Quick open (find files)" })
kset("n", "<leader>sf", action("workbench.action.quickOpen"), { desc = "Search files" })
kset("n", "<leader>sb", action("workbench.action.showAllEditors"), { desc = "Search open editors (buffers)" })
kset("n", "<leader>sg", action("workbench.action.findInFiles"), { desc = "Grep (find in files)" })
kset("n", "<leader>sC", action("workbench.action.showCommands"), { desc = "Search commands" })
kset("n", "<leader>s:", action("workbench.action.showCommands"), { desc = "Search command history" })
kset("n", "<leader>sk", action("workbench.action.openGlobalKeybindings"), { desc = "Search keymaps" })
kset("n", "<leader>sd", action("workbench.actions.view.problems"), { desc = "Search diagnostics (problems panel)" })
kset("n", "<leader>sD", action("workbench.action.showErrorsWarnings"), { desc = "Search buffer diagnostics" })
kset("n", "<leader>sh", action("workbench.action.openDocumentationUrl"), { desc = "Open documentation" })
kset("n", "<leader>sT", action("workbench.action.gotoSymbol"), { desc = "Search symbols in file" })
kset("n", "<leader>st", action("workbench.action.showAllSymbols"), { desc = "Search workspace symbols" })
kset("n", "<leader>sp", action("workbench.action.openRecent"), { desc = "Search projects / recent" })
kset("n", "<leader>sr", action("workbench.action.openRecent"), { desc = "Search recent files" })
kset({ "n", "x" }, "<leader>sw", function()
	vscode.action("workbench.action.findInFiles", {
		args = { query = vim.fn.expand("<cword>") },
	})
end, { desc = "Search word under cursor" })
kset("n", "<leader>sc", function()
	vscode.action("workbench.action.openSettingsJson")
end, { desc = "Open VSCode settings (config)" })

-- ──────────────────────────────────────────────────────────────────────────────
-- LSP
-- ──────────────────────────────────────────────────────────────────────────────

kset("n", "gd", action("editor.action.revealDefinition"), { desc = "Go to definition" })
kset("n", "gD", action("editor.action.revealDeclaration"), { desc = "Go to declaration" })
kset("n", "gr", action("editor.action.referenceSearch.trigger"), { desc = "References" })
kset("n", "gI", action("editor.action.goToImplementation"), { desc = "Go to implementation" })
kset("n", "gy", action("editor.action.goToTypeDefinition"), { desc = "Go to type definition" })
kset("n", "K", action("editor.action.showHover"), { desc = "Show hover documentation" })

-- ──────────────────────────────────────────────────────────────────────────────
-- Code actions
-- ──────────────────────────────────────────────────────────────────────────────

kset({ "n", "v" }, "<leader>ca", action("editor.action.quickFix"), { desc = "Code action" })
kset("n", "<leader>cr", action("editor.action.rename"), { desc = "Rename symbol" })
kset("n", "<leader>cf", action("editor.action.formatDocument"), { desc = "Format document" })
kset("n", "<leader>fb", action("editor.action.formatDocument"), { desc = "Format buffer" })
kset("x", "<leader>cf", action("editor.action.formatSelection"), { desc = "Format selection" })
kset({ "n", "v" }, "<leader>cl", action("editor.action.codelens.trigger"), { desc = "Run codelens" })

-- ──────────────────────────────────────────────────────────────────────────────
-- Diagnostics
-- ──────────────────────────────────────────────────────────────────────────────

kset("n", "[d", action("editor.action.marker.prev"), { desc = "Previous diagnostic" })
kset("n", "]d", action("editor.action.marker.next"), { desc = "Next diagnostic" })

-- ──────────────────────────────────────────────────────────────────────────────
-- Git / source control
-- ──────────────────────────────────────────────────────────────────────────────

kset("n", "<leader>gg", action("workbench.view.scm"), { desc = "Open source control (git)" })
kset("n", "]h", action("workbench.action.editor.nextChange"), { desc = "Next git hunk" })
kset("n", "[h", action("workbench.action.editor.previousChange"), { desc = "Previous git hunk" })
kset("n", "<leader>gs", action("git.stageSelectedRanges"), { desc = "Stage hunk" })
kset("n", "<leader>gu", action("git.revertSelectedRanges"), { desc = "Revert/unstage hunk" })
kset("n", "<leader>gd", action("git.openChange"), { desc = "Git diff current file" })

-- ──────────────────────────────────────────────────────────────────────────────
-- Notifications
-- ──────────────────────────────────────────────────────────────────────────────

kset("n", "<leader>nd", action("notifications.clearAll"), { desc = "Dismiss all notifications" })
-- Notification history (no direct VSCode equivalent — opens output panel)
kset("n", "<leader>nh", action("workbench.action.output.toggleOutput"), { desc = "Show output / notification log" })

-- ──────────────────────────────────────────────────────────────────────────────
-- Window / split management
-- ──────────────────────────────────────────────────────────────────────────────

kset("n", "<C-h>", action("workbench.action.focusLeftGroup"), { desc = "Focus left editor group" })
kset("n", "<C-l>", action("workbench.action.focusRightGroup"), { desc = "Focus right editor group" })
kset("n", "<C-k>", action("workbench.action.focusAboveGroup"), { desc = "Focus editor group above" })
kset("n", "<C-j>", action("workbench.action.focusBelowGroup"), { desc = "Focus editor group below" })

-- ──────────────────────────────────────────────────────────────────────────────
-- Misc / Utility
-- ──────────────────────────────────────────────────────────────────────────────

kset({ "n", "x" }, "<leader>r", function()
	vscode.with_insert(function()
		vscode.action("editor.action.refactor")
	end)
end, { desc = "Inline refactor" })

kset("n", "<leader><cr>", action("workbench.action.terminal.toggleTerminal"), { desc = "Toggle terminal" })

-- TODO navigation with the todo-tree extension
kset("n", "]t", action("todo-tree.goToNext"), { desc = "Next TODO comment" })
kset("n", "[t", action("todo-tree.goToPrevious"), { desc = "Previous TODO comment" })

kset("n", "x", [["_x]], { desc = "Delete without yanking" })
kset({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete without yanking" })

kset("n", "+", "<C-a>", { desc = "Increment number" })
kset("n", "-", "<C-x>", { desc = "Decrement number" })
