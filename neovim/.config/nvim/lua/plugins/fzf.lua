vim.pack.add({
	"https://github.com/nvim-mini/mini.icons",
	"https://github.com/ibhagwan/fzf-lua",
})

require("fzf-lua").setup({
	winopts = {
		backdrop = 100
	},
	grep = {
		hidden = true,
	},
	keymap = {
		fzf = {
			-- Send results to quickfix list
			["ctrl-q"] = "select-all+accept",
		},
	},
})

--- Keymaps
local kset = vim.keymap.set

kset("n", "<C-p>", function()
	FzfLua.vcs_files()
end, { desc = "Search project files" })

kset("n", "<leader>sc", function()
	FzfLua.files({ prompt = "Config files>", cwd = vim.fn.stdpath("config") })
end, { desc = "Search config files" })

kset("n", "<leader>sf", function()
	FzfLua.files()
end, { desc = "Search files" })

kset("n", "<leader>sb", function()
	FzfLua.buffers()
end, { desc = "Search buffers" })

kset("n", "<leader>sg", function()
	FzfLua.grep_project()
end, { desc = "Grep project" })

kset("n", "<leader>s:", function()
	FzfLua.command_history()
end, { desc = "Search command history" })

kset("n", "<leader>sC", function()
	FzfLua.commands()
end, { desc = "Search commands" })

kset("n", "<leader>sr", function()
	FzfLua.oldfiles()
end, { desc = "Search recent files" })

kset("n", "<leader>sw", function()
	FzfLua.grep_cword()
end, { desc = "Search word under cursor" })

kset("n", "<leader>sW", function()
	FzfLua.grep_cWORD()
end, { desc = "Search WORD under cursor" })

kset({ "v" }, "<leader>sw", function()
	FzfLua.grep_visual()
end, { desc = "Search visual selection" })

kset("n", '<leader>s"', function()
	FzfLua.registers()
end, { desc = "Search registers" })

kset("n", "<leader>s/", function()
	FzfLua.search_history()
end, { desc = "Search History" })

kset("n", "<leader>sd", function()
	FzfLua.diagnostics_document()
end, { desc = "Search document diagnostics" })

kset("n", "<leader>sD", function()
	FzfLua.diagnostics_workspace()
end, { desc = "Search workspace diagnostics" })

kset("n", "<leader>sh", function()
	FzfLua.helptags()
end, { desc = "Search help tags" })

kset("n", "<leader>sk", function()
	FzfLua.keymaps()
end, { desc = "Search keymaps" })

kset("n", "<leader>sm", function()
	FzfLua.marks()
end, { desc = "Search marks" })

kset("n", "<leader>sM", function()
	FzfLua.manpages()
end, { desc = "Search man pages" })

kset("n", "<leader>sl", function()
	FzfLua.loclist()
end, { desc = "Search location list" })

kset("n", "<leader>sq", function()
	FzfLua.quickfix()
end, { desc = "Search quickfix list" })

kset("n", "<leader>su", function()
	FzfLua.undotree()
end, { desc = "Search undo tree" })

kset("n", "<leader>st", function()
	FzfLua.lsp_document_symbols()
end, { desc = "Search LSP document symbols (tags)" })

kset("n", "<leader>sT", function()
	FzfLua.lsp_workspace_symbols()
end, { desc = "Search LSP workspace symbols (tags)" })

kset("n", "gd", function()
	FzfLua.lsp_definitions()
end, { desc = "Search LSP definitions" })

kset("n", "gD", function()
	FzfLua.lsp_declarations()
end, { desc = "Search LSP declarations" })

kset("n", "gr", function()
	FzfLua.lsp_references()
end, {
	nowait = true,
	desc = "Search LSP references",
})

kset("n", "gI", function()
	FzfLua.lsp_implementations()
end, { desc = "Search LSP implementations" })

kset("n", "gy", function()
	FzfLua.lsp_typedefs()
end, { desc = "Search LSP t[y]pe definitions" })
