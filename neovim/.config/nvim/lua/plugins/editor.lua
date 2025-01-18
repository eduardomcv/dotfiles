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
	-- A collection of UI plugins
	{
		"folke/snacks.nvim",
		dependencies = "mini.icons",
		priority = 1000,
		lazy = false,
		---@type snacks.Config
		opts = {
			bigfile = { enabled = true },
			git = { enabled = true },
			indent = { enabled = true },
			input = { enabled = true },
			notifier = { enabled = true },
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
				desc = "Git Blame Line",
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
	-- Show popup with available keybindings
	{
		"folke/which-key.nvim",
		dependencies = "mini.icons",
		event = "VeryLazy",
		opts = {
			spec = {
				{ "<leader>s", group = "+search" },
				{ "<leader>g", group = "+git" },
				{ "<leader>c", group = "+code" },
			},
		},
	},
	-- File explorer
	{
		"echasnovski/mini.files",
		version = "*",
		dependencies = "mini.icons",
		opts = {},
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
		config = true,
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
		config = true,
	},
}
