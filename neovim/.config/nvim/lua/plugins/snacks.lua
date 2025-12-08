-- A collection of QoL plugins
vim.pack.add({ "https://github.com/folke/snacks.nvim" })

require("snacks").setup({
	bigfile = { enabled = true },
	dashboard = {
		enabled = true,
		preset = {
			keys = {
				{ icon = " ", key = "n", desc = "New file", action = ":ene | startinsert" },
				{ icon = " ", key = "f", desc = "Search file", action = "<leader>sf" },
				{ icon = " ", key = "g", desc = "Search text", action = "<leader>sg" },
				{ icon = " ", key = "r", desc = "Recent files", action = "<leader>sr" },
				{ icon = " ", key = "c", desc = "Config", action = "<leader>sc" },
				{ icon = "󰟾 ", key = "m", desc = "Mason", action = ":Mason" },
				{ icon = " ", key = "q", desc = "Quit", action = ":qa" },
			},
			header = [[
|\---/|
| ,_, |
        \_`_/-..----.
         ___/ `   ' ,""+ \  sk
             (__...'   __\    |`.___.';
               (_,...'(_,.`__)/'.....+
     _   __                _          
   / | / /__  ____ _   __(_)___ ___ 
  /  |/ / _ \/ __ \ | / / / __ `__ \
 / /|  /  __/ /_/ / |/ / / / / / / /
/_/ |_/\___/\____/|___/_/_/ /_/ /_/ 
]],
		},
		formats = {
			key = function(item)
				return { { "[", hl = "special" }, { item.key, hl = "key" }, { "]", hl = "special" } }
			end,
		},
		sections = {
			{ section = "header" },
			{ icon = " ", title = "Keymaps", section = "keys", indent = 2, padding = 1 },
			{ icon = " ", title = "Recent files", section = "recent_files", indent = 2, padding = 1 },
			{ icon = " ", title = "Projects", section = "projects", indent = 2, padding = 1 },
		},
	},
	git = { enabled = true },
	gitbrowse = { enabled = true },
	indent = { enabled = true },
	lazygit = { enabled = true },
	notifier = { enabled = true },
	picker = {
		enabled = true,
		layout = {
			preset = "dropdown",
			hidden = {
				"preview",
			},
			layout = {
				backdrop = false,
				row = 4,
				height = 0.7,
			},
		},
		sources = {
			files = { hidden = true },
			grep = { hidden = true },
		},
	},
	quickfile = { enabled = true },
	scroll = { enabled = true },
	rename = { enabled = true },
	words = { enabled = true },
})

-- Create autocmd to integrate file renaming with mini.files
vim.api.nvim_create_autocmd("User", {
	pattern = "MiniFilesActionRename",
	callback = function(event)
		Snacks.rename.on_rename_file(event.data.from, event.data.to)
	end,
})

-- Disable scroll when entering insert mode
vim.api.nvim_create_autocmd("InsertEnter", {
	callback = function()
		vim.g.snacks_scroll = false
	end,
})

-- Enable scroll when leaving insert mode
vim.api.nvim_create_autocmd("InsertLeave", {
	callback = function()
		vim.g.snacks_scroll = true
	end,
})

vim.keymap.set("n", "<leader>gb", function()
	Snacks.git.blame_line()
end, { desc = "Git blame" })

vim.keymap.set({ "n", "v" }, "<leader>gB", function()
	Snacks.gitbrowse()
end, { desc = "Git browse" })

vim.keymap.set("n", "<leader>gg", function()
	Snacks.lazygit()
end, { desc = "Open Lazygit" })

vim.keymap.set({ "n", "t" }, "]]", function()
	Snacks.words.jump(vim.v.count1)
end, { desc = "Next reference" })

vim.keymap.set({ "n", "t" }, "[[", function()
	Snacks.words.jump(-vim.v.count1)
end, { desc = "Previous reference" })

vim.keymap.set("n", "<leader>nd", function()
	Snacks.notifier.hide()
end, { desc = "Dismiss all notifications" })
vim.keymap.set("n", "<leader>nh", function()
	Snacks.notifier.show_history()
end, { desc = "Notification history" })

vim.keymap.set("n", "<C-p>", function()
	Snacks.picker.smart()
end, { desc = "Smart find files" })

vim.keymap.set("n", "<leader>sc", function()
	Snacks.picker.files({ cwd = vim.fn.stdpath("config") })
end, { desc = "Search config files" })

vim.keymap.set("n", "<leader>sf", function()
	Snacks.picker.files()
end, { desc = "Search files" })

vim.keymap.set("n", "<leader>sb", function()
	Snacks.picker.buffers()
end, { desc = "Search buffers" })

vim.keymap.set("n", "<leader>sB", function()
	Snacks.picker.grep_buffers()
end, { desc = "Grep Open Buffers" })

vim.keymap.set("n", "<leader>sg", function()
	Snacks.picker.grep()
end, { desc = "Grep" })

vim.keymap.set("n", "<leader>s:", function()
	Snacks.picker.command_history()
end, { desc = "Search command history" })

vim.keymap.set("n", "<leader>sC", function()
	Snacks.picker.commands()
end, { desc = "Search commands" })

vim.keymap.set("n", "<leader>sn", function()
	Snacks.picker.notifications()
end, { desc = "Search notification history" })

vim.keymap.set("n", "<leader>sp", function()
	Snacks.picker.projects()
end, { desc = "Search projects" })

vim.keymap.set("n", "<leader>sr", function()
	Snacks.picker.recent()
end, { desc = "Search recent files" })

vim.keymap.set({ "n", "x" }, "<leader>sw", function()
	Snacks.picker.grep_word()
end, { desc = "Visual selection or word" })

vim.keymap.set("n", '<leader>s"', function()
	Snacks.picker.registers()
end, { desc = "Search registers" })

vim.keymap.set("n", "<leader>s/", function()
	Snacks.picker.search_history()
end, { desc = "Search History" })

vim.keymap.set("n", "<leader>sd", function()
	Snacks.picker.diagnostics()
end, { desc = "Search diagnostics" })

vim.keymap.set("n", "<leader>sD", function()
	Snacks.picker.diagnostics_buffer()
end, { desc = "Search buffer diagnostics" })

vim.keymap.set("n", "<leader>sh", function()
	Snacks.picker.help()
end, { desc = "Search help pages" })

vim.keymap.set("n", "<leader>sk", function()
	Snacks.picker.keymaps()
end, { desc = "Search keymaps" })

vim.keymap.set("n", "<leader>sm", function()
	Snacks.picker.marks()
end, { desc = "Search marks" })

vim.keymap.set("n", "<leader>sM", function()
	Snacks.picker.man()
end, { desc = "Search man pages" })

vim.keymap.set("n", "<leader>sl", function()
	Snacks.picker.loclist()
end, { desc = "Search location list" })

vim.keymap.set("n", "<leader>sq", function()
	Snacks.picker.qflist()
end, { desc = "Search quickfix list" })

vim.keymap.set("n", "<leader>su", function()
	Snacks.picker.undo()
end, { desc = "Search undo history" })

vim.keymap.set("n", "<leader>st", function()
	Snacks.picker.lsp_workspace_symbols()
end, { desc = "Search LSP Workspace Symbols (tags)" })

vim.keymap.set("n", "<leader>sT", function()
	Snacks.picker.lsp_symbols()
end, { desc = "Search LSP Symbols (tags)" })

vim.keymap.set("n", "gd", function()
	Snacks.picker.lsp_definitions()
end, { desc = "Goto Definition" })

vim.keymap.set("n", "gD", function()
	Snacks.picker.lsp_declarations()
end, { desc = "Goto Declaration" })

vim.keymap.set("n", "gr", function()
	Snacks.picker.lsp_references()
end, {
	nowait = true,
	desc = "References",
})

vim.keymap.set("n", "gI", function()
	Snacks.picker.lsp_implementations()
end, { desc = "Goto Implementation" })

vim.keymap.set("n", "gy", function()
	Snacks.picker.lsp_type_definitions()
end, { desc = "Goto T[y]pe Definition" })
