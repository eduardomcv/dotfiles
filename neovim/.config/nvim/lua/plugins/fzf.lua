vim.pack.add({
	"https://github.com/nvim-mini/mini.icons",
	"https://github.com/ibhagwan/fzf-lua",
})

require("fzf-lua").setup({
	keymap = {
		fzf = {
			["ctrl-q"] = "select-all+accept",
		},
	},
})

vim.keymap.set("n", "<C-p>", function()
	FzfLua.vcs_files()
end, { desc = "Search project files" })

vim.keymap.set("n", "<leader>sc", function()
	FzfLua.files({ prompt = "Config files>", cwd = vim.fn.stdpath("config") })
end, { desc = "Search config files" })

vim.keymap.set("n", "<leader>sf", function()
	FzfLua.files()
end, { desc = "Search files" })

vim.keymap.set("n", "<leader>sb", function()
	FzfLua.buffers()
end, { desc = "Search buffers" })

vim.keymap.set("n", "<leader>sg", function()
	FzfLua.grep_project()
end, { desc = "Grep project" })

vim.keymap.set("n", "<leader>s:", function()
	FzfLua.command_history()
end, { desc = "Search command history" })

vim.keymap.set("n", "<leader>sC", function()
	FzfLua.commands()
end, { desc = "Search commands" })

vim.keymap.set("n", "<leader>sr", function()
	FzfLua.oldfiles()
end, { desc = "Search recent files" })

vim.keymap.set("n", "<leader>sw", function()
	FzfLua.grep_cword()
end, { desc = "Search word under cursor" })

vim.keymap.set("n", "<leader>sW", function()
	FzfLua.grep_cWORD()
end, { desc = "Search WORD under cursor" })

vim.keymap.set({ "v" }, "<leader>sw", function()
	FzfLua.grep_visual()
end, { desc = "Search visual selection" })

vim.keymap.set("n", '<leader>s"', function()
	FzfLua.registers()
end, { desc = "Search registers" })

vim.keymap.set("n", "<leader>s/", function()
	FzfLua.search_history()
end, { desc = "Search History" })

vim.keymap.set("n", "<leader>sd", function()
	FzfLua.diagnostics_document()
end, { desc = "Search document diagnostics" })

vim.keymap.set("n", "<leader>sD", function()
	FzfLua.diagnostics_workspace()
end, { desc = "Search workspace diagnostics" })

vim.keymap.set("n", "<leader>sh", function()
	FzfLua.helptags()
end, { desc = "Search help tags" })

vim.keymap.set("n", "<leader>sk", function()
	FzfLua.keymaps()
end, { desc = "Search keymaps" })

vim.keymap.set("n", "<leader>sm", function()
	FzfLua.marks()
end, { desc = "Search marks" })

vim.keymap.set("n", "<leader>sM", function()
	FzfLua.manpages()
end, { desc = "Search man pages" })

vim.keymap.set("n", "<leader>sl", function()
	FzfLua.loclist()
end, { desc = "Search location list" })

vim.keymap.set("n", "<leader>sq", function()
	FzfLua.quickfix()
end, { desc = "Search quickfix list" })

vim.keymap.set("n", "<leader>su", function()
	FzfLua.undotree()
end, { desc = "Search undo tree" })

vim.keymap.set("n", "<leader>st", function()
	FzfLua.lsp_document_symbols()
end, { desc = "Search LSP document symbols (tags)" })

vim.keymap.set("n", "<leader>sT", function()
	FzfLua.lsp_workspace_symbols()
end, { desc = "Search LSP workspace symbols (tags)" })

vim.keymap.set("n", "gd", function()
	FzfLua.lsp_definitions()
end, { desc = "Search LSP definitions" })

vim.keymap.set("n", "gD", function()
	FzfLua.lsp_declarations()
end, { desc = "Search LSP declarations" })

vim.keymap.set("n", "gr", function()
	FzfLua.lsp_references()
end, {
	nowait = true,
	desc = "Search LSP references",
})

vim.keymap.set("n", "gI", function()
	FzfLua.lsp_implementations()
end, { desc = "Search LSP implementations" })

vim.keymap.set("n", "gy", function()
	FzfLua.lsp_typedefs()
end, { desc = "Search LSP t[y]pe definitions" })
