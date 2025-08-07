if IS_VSCODE then
	-- Don't load editor plugins inside vscode
	return {}
end

return {
	-- Icons
	{
		"echasnovski/mini.icons",
		version = "*",
		lazy = true,
		config = function()
			require("mini.icons").setup()
			MiniIcons.mock_nvim_web_devicons()
		end,
	},
	-- A collection of QoL plugins
	{
		"folke/snacks.nvim",
		dependencies = {
			"mini.icons",
		},
		priority = 1000,
		lazy = false,
		opts = {
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
						{ icon = "󰒲 ", key = "l", desc = "Lazy", action = ":Lazy", enabled = package.loaded.lazy ~= nil },
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
			input = { enabled = true },
			notifier = { enabled = true },
			picker = {
				enabled = true,
				sources = {
					files = { hidden = true },
					grep = { hidden = true },
				},
			},
			quickfile = { enabled = true },
			scroll = { enabled = true },
			rename = { enabled = true },
			words = { enabled = true },
		},
		keys = {
			{
				"<leader>gb",
				function()
					Snacks.git.blame_line()
				end,
				desc = "Git blame",
			},
			{
				"<leader>gB",
				function()
					Snacks.gitbrowse()
				end,
				desc = "Git browse",
				mode = { "n", "v" },
			},
			{
				"]]",
				function()
					Snacks.words.jump(vim.v.count1)
				end,
				desc = "Next reference",
				mode = { "n", "t" },
			},
			{
				"[[",
				function()
					Snacks.words.jump(-vim.v.count1)
				end,
				desc = "Previous reference",
				mode = { "n", "t" },
			},
			{
				"<leader>nd",
				function()
					Snacks.notifier.hide()
				end,
				desc = "Dismiss all notifications",
			},
			{
				"<leader>nh",
				function()
					Snacks.notifier.show_history()
				end,
				desc = "Notification history",
			},
			{
				"<C-p>",
				function()
					Snacks.picker.smart()
				end,
				desc = "Smart find files",
			},
			{
				"<leader>sc",
				function()
					Snacks.picker.files({ cwd = vim.fn.stdpath("config") })
				end,
				desc = "Search config files",
			},
			{
				"<leader>sf",
				function()
					Snacks.picker.files()
				end,
				desc = "Search files",
			},
			{
				"<leader>sb",
				function()
					Snacks.picker.buffers()
				end,
				desc = "Search buffers",
			},
			{
				"<leader>sB",
				function()
					Snacks.picker.grep_buffers()
				end,
				desc = "Grep Open Buffers",
			},
			{
				"<leader>sg",
				function()
					Snacks.picker.grep()
				end,
				desc = "Grep",
			},
			{
				"<leader>s:",
				function()
					Snacks.picker.command_history()
				end,
				desc = "Search command history",
			},
			{
				"<leader>sC",
				function()
					Snacks.picker.commands()
				end,
				desc = "Search commands",
			},
			{
				"<leader>sn",
				function()
					Snacks.picker.notifications()
				end,
				desc = "Search notification history",
			},
			{
				"<leader>sp",
				function()
					Snacks.picker.projects()
				end,
				desc = "Search projects",
			},
			{
				"<leader>sr",
				function()
					Snacks.picker.recent()
				end,
				desc = "Search recent files",
			},
			{
				"<leader>sw",
				function()
					Snacks.picker.grep_word()
				end,
				desc = "Visual selection or word",
				mode = { "n", "x" },
			},
			{
				'<leader>s"',
				function()
					Snacks.picker.registers()
				end,
				desc = "Search registers",
			},
			{
				"<leader>s/",
				function()
					Snacks.picker.search_history()
				end,
				desc = "Search History",
			},
			{
				"<leader>sd",
				function()
					Snacks.picker.diagnostics()
				end,
				desc = "Search diagnostics",
			},
			{
				"<leader>sD",
				function()
					Snacks.picker.diagnostics_buffer()
				end,
				desc = "Search buffer diagnostics",
			},
			{
				"<leader>sh",
				function()
					Snacks.picker.help()
				end,
				desc = "Search help pages",
			},
			{
				"<leader>sk",
				function()
					Snacks.picker.keymaps()
				end,
				desc = "Search keymaps",
			},
			{
				"<leader>sm",
				function()
					Snacks.picker.marks()
				end,
				desc = "Search marks",
			},
			{
				"<leader>sM",
				function()
					Snacks.picker.man()
				end,
				desc = "Search man pages",
			},
			{
				"<leader>sl",
				function()
					Snacks.picker.loclist()
				end,
				desc = "Search location list",
			},
			{
				"<leader>sq",
				function()
					Snacks.picker.qflist()
				end,
				desc = "Search quickfix list",
			},
			{
				"<leader>su",
				function()
					Snacks.picker.undo()
				end,
				desc = "Search undo history",
			},
			{
				"<leader>st",
				function()
					Snacks.picker.lsp_workspace_symbols()
				end,
				desc = "Search LSP Workspace Symbols (tags)",
			},
			{
				"<leader>sT",
				function()
					Snacks.picker.lsp_symbols()
				end,
				desc = "Search LSP Symbols (tags)",
			},
			{
				"gd",
				function()
					Snacks.picker.lsp_definitions()
				end,
				desc = "Goto Definition",
			},
			{
				"gD",
				function()
					Snacks.picker.lsp_declarations()
				end,
				desc = "Goto Declaration",
			},
			{
				"gr",
				function()
					Snacks.picker.lsp_references()
				end,
				nowait = true,
				desc = "References",
			},
			{
				"gI",
				function()
					Snacks.picker.lsp_implementations()
				end,
				desc = "Goto Implementation",
			},
			{
				"gy",
				function()
					Snacks.picker.lsp_type_definitions()
				end,
				desc = "Goto T[y]pe Definition",
			},
		},
		init = function()
			-- Create autocmd to integrate file renaming with mini.files
			vim.api.nvim_create_autocmd("User", {
				pattern = "MiniFilesActionRename",
				callback = function(event)
					Snacks.rename.on_rename_file(event.data.from, event.data.to)
				end,
			})
		end,
	},
	-- Better UI for messages, cmdline and popupmenu
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		dependencies = {
			-- UI component library
			{ "MunifTanjim/nui.nvim", lazy = true },
		},
		opts = {
			cmdline = {
				opts = {
					position = {
						row = "30%",
					},
				},
			},
			presets = {
				bottom_search = false,
				command_palette = true,
				long_message_to_split = true,
				lsp_doc_border = true,
			},
			lsp = {
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
				},
			},
		},
	},
	-- Show pop-up with available keybindings
	{
		"folke/which-key.nvim",
		dependencies = "mini.icons",
		event = "VeryLazy",
		opts = {
			spec = {
				{ "<leader>s", group = "+search" },
				{ "<leader>g", group = "+git" },
				{ "<leader>c", group = "+code" },
				{ "<leader>n", group = "+notification" },
			},
		},
	},
	-- File explorer
	{
		"echasnovski/mini.files",
		version = "*",
		dependencies = {
			"mini.icons",
		},
		opts = {
			content = {
				-- Filter these files from the explorer
				filter = function(entry)
					return entry.name ~= ".DS_Store" and entry.name ~= ".git" and entry.name ~= ".direnv"
				end,
				sort = function(entries)
					-- We can take advantage of having all the entries available here
					-- in the sort function to send all the paths at once to `git check-ignore`.
					-- Using the filter function above, we would have to run the command once for each entry.
					local all_paths = table.concat(
						vim.tbl_map(function(entry)
							return entry.path
						end, entries),
						"\n"
					)

					local output_lines = {}

					-- Run `git check-ignore` command waiting for stdin
					local job_id = vim.fn.jobstart({ "git", "check-ignore", "--stdin" }, {
						stdout_buffered = true,
						on_stdout = function(_, data)
							output_lines = data
						end,
					})

					-- Command failed to run
					if job_id < 1 then
						return entries
					end

					-- Send all paths to the command via stdin
					vim.fn.chansend(job_id, all_paths)
					vim.fn.chanclose(job_id, "stdin")
					vim.fn.jobwait({ job_id })

					-- Filter out git ignored files.
					local filtered_entries = vim.tbl_filter(function(entry)
						return not vim.tbl_contains(output_lines, entry.path)
					end, entries)

					return require("mini.files").default_sort(filtered_entries)
				end,
			},
		},
		keys = {
			{
				"<leader>e",
				function()
					local MiniFiles = require("mini.files")
					if not MiniFiles.close() then
						MiniFiles.open(vim.api.nvim_buf_get_name(0))
						MiniFiles.reveal_cwd()
					end
				end,
				desc = "Toggle file explorer (current buffer)",
			},
			{
				"<leader>r",
				function()
					local MiniFiles = require("mini.files")
					if not MiniFiles.close() then
						MiniFiles.open()
					end
				end,
				desc = "Toggle file explorer (root dir)",
			},
		},
	},
	-- Integrate tmux navigation
	{
		"christoomey/vim-tmux-navigator",
		cmd = {
			"TmuxNavigateLeft",
			"TmuxNavigateDown",
			"TmuxNavigateUp",
			"TmuxNavigateRight",
			"TmuxNavigatePrevious",
			"TmuxNavigatorProcessList",
		},
		keys = {
			{ "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
			{ "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
			{ "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
			{ "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
			{ "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
		},
	},
	-- Git UI similar to magit
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"folke/snacks.nvim",
			{
				"sindrets/diffview.nvim",
				opts = {
					view = {
						merge_tool = {
							layout = "diff3_mixed",
						},
					},
				},
			},
		},
		opts = {
			integrations = {
				diffview = true,
				snacks = true,
			},
		},
		keys = {
			{
				"<leader>gg",
				function()
					require("neogit").open()
				end,
				desc = "Open neogit",
			},
			{
				"<leader>gs",
				function()
					require("neogit").open({ kind = "split" })
				end,
				desc = "Open neogit (split window)",
			},
			{
				"<leader>gv",
				function()
					require("neogit").open({ kind = "vsplit" })
				end,
				desc = "Open neogit (vertical split window)",
			},
		},
	},
	-- Tracking of git data and exposes :Git command
	{
		"echasnovski/mini-git",
		version = "*",
		main = "mini.git",
		opts = {},
	},
	-- Integrates git diffs and hunks
	{
		"echasnovski/mini.diff",
		version = "*",
		opts = {
			view = {
				style = "sign",
			},
		},
	},
	-- Status line
	{
		"echasnovski/mini.statusline",
		version = "*",
		dependencies = {
			"mini.icons",
			"mini-git",
			"mini.diff",
		},
		opts = {},
	},
	-- Extends forward/back functionality with brackets
	{
		"echasnovski/mini.bracketed",
		version = "*",
		opts = {},
	},
}
