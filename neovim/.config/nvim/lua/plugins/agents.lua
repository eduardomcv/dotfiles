vim.pack.add({
	"https://github.com/MeanderingProgrammer/render-markdown.nvim",
	"https://github.com/sudo-tee/opencode.nvim",
})

require("render-markdown").setup({
	preset = "lazy",
	file_types = { "markdown", "opencode_output" },
	latex = { enabled = false },
	anti_conceal = { enabled = false },
})

require("opencode").setup({
	preferred_picker = "snacks",
	preferred_completion = "blink",
	default_mode = "plan",
	keymap_prefix = "<leader>a",
	quick_chat = {
		default_model = "gpt-4o",
	},
	ui = {
		window_width = 0.25,
	},
})
