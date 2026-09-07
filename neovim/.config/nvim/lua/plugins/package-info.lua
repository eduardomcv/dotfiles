vim.pack.add({
	"https://github.com/MunifTanjim/nui.nvim",
	"https://github.com/folke/which-key.nvim",
	"https://github.com/vuki656/package-info.nvim",
})

require("package-info").setup()

vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
	group = vim.api.nvim_create_augroup("PackageInfoKeymaps", { clear = true }),
	pattern = "package.json",
	callback = function(event)
		require("which-key").add({
			{
				"<leader>p",
				group = "+package",
				buffer = event.buf,
			},
		})

		local function set(mode, lhs, rhs, desc)
			vim.keymap.set(mode, lhs, rhs, {
				buffer = event.buf,
				silent = true,
				desc = desc,
			})
		end

		set("n", "<leader>pp", "<cmd>PackageInfoToggle<cr>", "Toggle package info")
		set("n", "<leader>ps", "<cmd>PackageInfoShow<cr>", "Show package info")
		set("n", "<leader>ph", "<cmd>PackageInfoHide<cr>", "Hide package info")
		set("n", "<leader>pi", "<cmd>PackageInfoInstall<cr>", "Install package")
		set("n", "<leader>pd", "<cmd>PackageInfoDelete<cr>", "Delete package")
		set("n", "<leader>pc", "<cmd>PackageInfoChangeVersion<cr>", "Change package version")
		set("n", "<leader>pu", "<cmd>PackageInfoUpdate<cr>", "Update package")
	end,
})
