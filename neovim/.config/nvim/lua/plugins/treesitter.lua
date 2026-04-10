vim.pack.add({
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/windwp/nvim-ts-autotag",
})

local treesitter = require("nvim-treesitter")

treesitter.install({
	"bash",
	"lua",
	"regex",
	"vim",
	"yaml",
	"toml",
	"json",
	"markdown",
	"markdown_inline",
	"html",
	"css",
	"javascript",
	"typescript",
	"tsx",
	"make",
	"python",
})

require("nvim-ts-autotag").setup({})

-- Autocmds

-- Automatically run :TSUpdate when nvim-treesitter is updated via vim.pack.update()
vim.api.nvim_create_autocmd("PackChanged", {
	desc = "Handle nvim-treesitter updates",
	group = vim.api.nvim_create_augroup("nvim-treesitter-pack-changed-update-handler", { clear = true }),
	callback = function(event)
		if event.data.kind == "update" and event.data.spec.name == "nvim-treesitter" then
			vim.notify("nvim-treesitter updated, running TSUpdate...", vim.log.levels.INFO)
			---@diagnostic disable-next-line: param-type-mismatch
			local ok = pcall(vim.cmd, "TSUpdate")
			if ok then
				vim.notify("TSUpdate completed successfully!", vim.log.levels.INFO)
			else
				vim.notify("TSUpdate command not available yet, skipping", vim.log.levels.WARN)
			end
		end
	end,
})
