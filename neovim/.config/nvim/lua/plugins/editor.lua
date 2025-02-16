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
		dependencies = "mini.icons",
		priority = 1000,
		lazy = false,
		opts = {
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
						{ icon = "󱉼 ", key = "v", desc = "Search vault", action = "<leader>sv" },
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
	-- UI component library
	{ "MunifTanjim/nui.nvim", lazy = true },
	-- Better UI for messages, cmdline and popupmenu
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		dependencies = {
			"nui.nvim",
		},
		opts = {
			lsp = {
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
					["cmp.entry.get_documentation"] = true,
				},
			},
			presets = {
				bottom_search = false,
				command_palette = true,
				long_message_to_split = true,
				lsp_doc_border = true,
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
				{ "<leader>o", group = "+obsidian" },
				{ "<leader>od", group = "+dailies" },
			},
		},
	},
	-- File explorer
	{
		"echasnovski/mini.files",
		version = "*",
		dependencies = "mini.icons",
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
						MiniFiles.open()
					end
				end,
				desc = "Toggle file explorer",
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
			"sindrets/diffview.nvim",
			"nvim-telescope/telescope.nvim",
		},
		opts = {
			integrations = {
				telescope = true,
				diffview = true,
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
	-- Support for obsidian
	{
		"epwalsh/obsidian.nvim",
		version = "*",
		lazy = true,
		-- Only trigger when inside vault directories
		event = {
			"BufReadPre " .. vim.g.obsidian_vault_dir .. "*.md",
			"BufNewFile " .. vim.g.obsidian_vault_dir .. "*.md",
		},
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		opts = {
			workspaces = {
				{
					name = "obsidian-vault",
					path = vim.g.obsidian_vault_dir,
				},
			},
			notes_subdir = "Unsorted",
			new_notes_location = "notes_subdir",
			daily_notes = {
				folder = "Daily notes",
				default_tags = { "daily-notes" },
				template = "Templates/Daily template",
			},
			templates = {
				folder = "Templates",
			},
			completion = {
				nvim_cmp = true,
				min_chars = 1,
			},
			-- Opt out of using ids
			note_id_func = function(title)
				return title
			end,
			-- Customise note file path
			note_path_func = function(spec)
				local path

				if spec.title then
					path = spec.dir / tostring(spec.title)
				else
					path = spec.dir / tostring(spec.id)
				end

				return path:with_suffix(".md")
			end,
			-- Customize the frontmatter data
			note_frontmatter_func = function(note)
				-- Opt out of using ids in general
				local out = { tags = note.tags }

				-- `note.metadata` contains any manually added fields in the frontmatter.
				-- So here we just make sure those fields are kept in the frontmatter.
				if note.metadata ~= nil and not vim.tbl_isempty(note.metadata) then
					for k, v in pairs(note.metadata) do
						out[k] = v
					end
				end

				return out
			end,
		},
		keys = {
			{ "<leader>on", "<cmd>ObsidianNew<cr>", desc = "Create new note" },
			{ "<leader>odd", "<cmd>ObsidianDailies<cr>", desc = "Browse daily notes" },
			{ "<leader>odt", "<cmd>ObsidianToday<cr>", desc = "Open or create today's daily note" },
			{ "<leader>ody", "<cmd>ObsidianYesterday<cr>", desc = "Open or create yesterday's daily note" },
			{ "<leader>odm", "<cmd>ObsidianTomorrow<cr>", desc = "Open or create tomorrow's daily note" },
			{ "<leader>ob", "<cmd>ObsidianBacklinks<cr>", desc = "Browse note backlinks" },
			{ "<leader>og", "<cmd>ObsidianSearch<cr>", desc = "Grep notes" },
			{ "<leader>oq", "<cmd>ObsidianQuickSwitch<cr>", desc = "Quick switch notes" },
			{ "<leader>oo", "<cmd>ObsidianOpen<cr>", desc = "Open note in Obsidian" },
			{ "<leader>or", "<cmd>ObsidianRename<cr>", desc = "Rename note" },
			{ "<leader>oc", "<cmd>ObsidianTOC<cr>", desc = "Browse table of contents" },
			{ "<leader>ow", "<cmd>ObsidianWorkspace<cr>", desc = "Browse workspaces" },
			{ "<leader>oe", "<cmd>ObsidianExtractNote<cr>", mode = "v", desc = "Extract note" },
			{ "<leader>ot", "<cmd>ObsidianTemplate<cr>", mode = "v", desc = "Apply template to note" },
		},
	},
}
