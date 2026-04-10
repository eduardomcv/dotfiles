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
	lazygit = { enabled = false },
	notifier = { enabled = true },
	quickfile = { enabled = true },
	scroll = { enabled = true },
	rename = { enabled = true },
	words = { enabled = true },
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
