vim.pack.add({
	-- { src = "https://github.com/nvim-lua/plenary.nvim", version = "master" },
	-- { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
	-- { src = "https://github.com/hakonharnes/img-clip.nvim" },
	-- { src = "https://github.com/ravitemer/codecompanion-history.nvim" },
	-- { src = "https://github.com/olimorris/codecompanion.nvim" },
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
