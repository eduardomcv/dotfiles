vim.pack.add({
	{ src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	-- UI component library
	"https://github.com/MunifTanjim/nui.nvim",
	-- Better UI for messages, cmdline and popupmenu
	"https://github.com/folke/noice.nvim",
	-- Show pop-up with available keybindings
	"https://github.com/folke/which-key.nvim",
	-- Git conflicts visualizer
	{ src = "https://github.com/akinsho/git-conflict.nvim", version = vim.version.range("*") },
	-- Improve tab integration
	"https://github.com/nanozuki/tabby.nvim",
	-- Highlight TODO comments
	"https://github.com/folke/todo-comments.nvim",
	-- Highlight colors
	"https://github.com/brenoprata10/nvim-highlight-colors",
})

require("noice").setup({
	presets = {

		command_palette = true,
		long_message_to_split = true,
		lsp_doc_border = true,
	},
	lsp = {
		override = {
			["vim.lsp.util.convert_input_to_markdown_lines"] = true,
			["vim.lsp.util.stylize_markdown"] = true,
		},
		progress = {
			view = "notify",
		},
	},
	views = {
		cmdline_popup = {
			position = {
				row = "20%",
			},
		},
		notify = {
			replace = true,
		},
	},
	routes = {
		{
			view = "notify",
			filter = { event = "msg_showmode" },
		},
	},
})

require("which-key").setup({
	spec = {
		{ "<leader>c", group = "+code" },
		{ "<leader>f", group = "+format" },
		{ "<leader>g", group = "+git" },
		{ "<leader>n", group = "+notification" },
		{ "<leader>s", group = "+search" },
	},
})

---@diagnostic disable-next-line: missing-fields
require("git-conflict").setup({})

require("todo-comments").setup({})

require("tabby").setup()

local function rename_tab()
	local tab_name = vim.fn.input({ prompt = "New tab name" })
	if tab_name ~= "" then
		require("tabby").tab_rename(tab_name)
	end
end

vim.keymap.set("n", "<leader>tr", rename_tab, { desc = "Rename tab" })

require("nvim-highlight-colors").setup({})
