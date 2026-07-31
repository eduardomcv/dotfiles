vim.pack.add({
	"https://github.com/mason-org/mason.nvim",
})

require("mason").setup({})
local registry = require("mason-registry")

local packages_to_install = {
	"lua-language-server",
	"luacheck",
	"stylua",
	"bash-language-server",
	"shfmt",
	"vtsls",
	"eslint-lsp",
	"css-lsp",
	"html-lsp",
	"emmet-language-server",
	"ty",
	"debugpy",
	"json-lsp",
	"yaml-language-server",
	"copilot-language-server",
	"js-debug-adapter",
}

registry.refresh(function()
	for _, pkg_name in ipairs(packages_to_install) do
		local ok, pkg = pcall(registry.get_package, pkg_name)
		if ok and not pkg:is_installed() then
			pkg:install()
		end
	end
end)
