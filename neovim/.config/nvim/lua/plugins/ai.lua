vim.pack.add({
	"https://github.com/MeanderingProgrammer/render-markdown.nvim",
	"https://github.com/sudo-tee/opencode.nvim",
})

require("render-markdown").setup({
	anti_conceal = { enabled = false },
	file_types = { "markdown", "opencode_output" },
})

require("opencode").setup({
	preferred_picker = "snacks",
	preferred_completion = "blink",
	default_mode = "plan",
})
