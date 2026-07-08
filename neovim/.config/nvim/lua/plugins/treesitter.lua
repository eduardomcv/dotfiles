vim.pack.add({
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/windwp/nvim-ts-autotag",
})

require("nvim-treesitter").install({
	"lua",
	"vim",
	"vimdoc",
	"bash",
	"zsh",
	"json",
	"yaml",
	"toml",
	"make",
	"dockerfile",
	"gitignore",
	"markdown",
	"markdown_inline",
	"html",
	"css",
	"javascript",
	"typescript",
	"tsx",
	"python",
	"dart",
})

require("nvim-ts-autotag").setup({})

--- Autocmds

vim.api.nvim_create_autocmd("PackChanged", {
	desc = "Automatically run :TSUpdate when the nvim-treesitter plugin is updated",
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

vim.api.nvim_create_autocmd("FileType", {
	desc = "Enable treesitter, except for huge files to save memory",
	callback = function(args)
		local buf = args.buf
		local max_filesize = 100 * 1024 -- 100 KB

		local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))

		if ok and stats and stats.size > max_filesize then
			-- Fallback to regex syntax highlighting, skip building the AST
			vim.bo[buf].syntax = "on"
			return
		end

		pcall(vim.treesitter.start, buf)
	end,
})
