vim.pack.add({ "https://github.com/folke/snacks.nvim" })

require("snacks").setup({
	bigfile = { enabled = true },
	git = { enabled = true },
	gitbrowse = { enabled = true },
	indent = { enabled = true },
	input = { enabled = true },
	notifier = { enabled = true },
	quickfile = { enabled = true },
	scroll = { enabled = true },
	rename = { enabled = true },
	words = { enabled = true },
	dashboard = {
		enabled = true,
		preset = {
			keys = {
				{ icon = " ", key = "n", desc = "New file", action = ":ene | startinsert" },
				{ icon = " ", key = "f", desc = "Search file", action = "<leader>sf" },
				{ icon = " ", key = "g", desc = "Search text", action = "<leader>sg" },
				{ icon = " ", key = "r", desc = "Recent files", action = "<leader>sr" },
				{ icon = " ", key = "c", desc = "Config", action = "<leader>sc" },
				{ icon = " ", key = "q", desc = "Quit", action = ":qa" },
			},
			-- stylua: ignore start
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
		-- stylua: ignore end
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
	picker = {
		enabled = true,
		layout = {
			hidden = {
				"preview",
			},
			preset = "dropdown",
			layout = {
				row = 4,
				max_height = math.floor(vim.o.lines * 0.8),
				width = math.floor(vim.o.columns * 0.5),
			},
		},
		sources = {
			files = { hidden = true },
			grep = { hidden = true },
		},
	},
})

--- Autocmds

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

--- Keymaps
local kset = vim.keymap.set

kset("n", "<leader>gb", function()
	Snacks.git.blame_line()
end, { desc = "Git blame" })

kset({ "n", "v" }, "<leader>gB", function()
	Snacks.gitbrowse()
end, { desc = "Git browse" })

kset({ "n", "t" }, "]]", function()
	Snacks.words.jump(vim.v.count1)
end, { desc = "Next reference" })

kset({ "n", "t" }, "[[", function()
	Snacks.words.jump(-vim.v.count1)
end, { desc = "Previous reference" })

kset("n", "<leader>nd", function()
	Snacks.notifier.hide()
end, { desc = "Dismiss all notifications" })

kset("n", "<leader>nh", function()
	Snacks.notifier.show_history()
end, { desc = "Notification history" })

kset("n", "<C-p>", function()
	Snacks.picker.smart()
end, { desc = "Smart find files" })

kset("n", "<leader>sc", function()
	Snacks.picker.files({ cwd = vim.fn.stdpath("config") })
end, { desc = "Search config files" })

kset("n", "<leader>sf", function()
	Snacks.picker.files()
end, { desc = "Search files" })

kset("n", "<leader>sb", function()
	Snacks.picker.buffers()
end, { desc = "Search buffers" })

kset("n", "<leader>sB", function()
	Snacks.picker.grep_buffers()
end, { desc = "Grep Open Buffers" })

kset("n", "<leader>sg", function()
	Snacks.picker.grep({ layout = { hidden = {} } })
end, { desc = "Grep" })

kset("n", "<leader>s:", function()
	Snacks.picker.command_history()
end, { desc = "Search command history" })

kset("n", "<leader>sC", function()
	Snacks.picker.commands()
end, { desc = "Search commands" })

kset("n", "<leader>sn", function()
	Snacks.picker.notifications()
end, { desc = "Search notification history" })

kset("n", "<leader>sp", function()
	Snacks.picker.projects()
end, { desc = "Search projects" })

kset("n", "<leader>sr", function()
	Snacks.picker.recent()
end, { desc = "Search recent files" })

kset({ "n", "x" }, "<leader>sw", function()
	Snacks.picker.grep_word()
end, { desc = "Visual selection or word" })

kset("n", '<leader>s"', function()
	Snacks.picker.registers()
end, { desc = "Search registers" })

kset("n", "<leader>s/", function()
	Snacks.picker.search_history()
end, { desc = "Search History" })

kset("n", "<leader>sd", function()
	Snacks.picker.diagnostics()
end, { desc = "Search diagnostics" })

kset("n", "<leader>sD", function()
	Snacks.picker.diagnostics_buffer()
end, { desc = "Search buffer diagnostics" })

kset("n", "<leader>sh", function()
	Snacks.picker.help()
end, { desc = "Search help pages" })

kset("n", "<leader>sk", function()
	Snacks.picker.keymaps()
end, { desc = "Search keymaps" })

kset("n", "<leader>sm", function()
	Snacks.picker.marks()
end, { desc = "Search marks" })

kset("n", "<leader>sM", function()
	Snacks.picker.man()
end, { desc = "Search man pages" })

kset("n", "<leader>sl", function()
	Snacks.picker.loclist()
end, { desc = "Search location list" })

kset("n", "<leader>sq", function()
	Snacks.picker.qflist()
end, { desc = "Search quickfix list" })

kset("n", "<leader>su", function()
	Snacks.picker.undo()
end, { desc = "Search undo history" })

kset("n", "<leader>st", function()
	Snacks.picker.lsp_workspace_symbols()
end, { desc = "Search LSP Workspace Symbols (tags)" })

kset("n", "<leader>sT", function()
	Snacks.picker.lsp_symbols()
end, { desc = "Search LSP Symbols (tags)" })

kset("n", "gd", function()
	Snacks.picker.lsp_definitions()
end, { desc = "Goto Definition" })

kset("n", "gD", function()
	Snacks.picker.lsp_declarations()
end, { desc = "Goto Declaration" })

kset("n", "gr", function()
	Snacks.picker.lsp_references()
end, {
	nowait = true,
	desc = "References",
})

kset("n", "gI", function()
	Snacks.picker.lsp_implementations()
end, { desc = "Goto Implementation" })

kset("n", "gy", function()
	Snacks.picker.lsp_type_definitions()
end, { desc = "Goto T[y]pe Definition" })
