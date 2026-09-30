use({ "nickjvandyke/opencode.nvim" })

local set = vim.keymap.set

set({ "n", "x" }, "<leader>oa", function()
	require("opencode").ask("@this: ")
end, { desc = "Ask OpenCode…" })

set({ "n", "x" }, "<leader>oo", function()
	require("opencode").select()
end, { desc = "Select OpenCode…" })

set("x", "go", function()
	return require("opencode").operator("@this")
end, { desc = "Send range to OpenCode", expr = true })

set("n", "goo", function()
	return require("opencode").operator("@this") .. "_"
end, { desc = "Send line to OpenCode", expr = true })
