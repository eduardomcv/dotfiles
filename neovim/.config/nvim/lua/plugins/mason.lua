vim.pack.add({ "https://github.com/mason-org/mason.nvim" })

require("mason").setup()

local registry = require("mason-registry")

local packages_to_install = {
	"lua-language-server",
	"luacheck",
	"stylua",
	"bash-language-server",
	"shfmt",
	"shellcheck",
	"vtsls",
	"eslint-lsp",
	"css-lsp",
	"html-lsp",
	"emmet-language-server",
	"ty",
	"ruff",
	"debugpy",
	"json-lsp",
	"yaml-language-server",
	"copilot-language-server",
	"js-debug-adapter",
	"markdownlint",
	"prettier",
}

local function ensure_packages_installed()
	registry.refresh(function()
		for _, pkg_name in ipairs(packages_to_install) do
			local ok, pkg = pcall(registry.get_package, pkg_name)
			if ok and not pkg:is_installed() then
				pkg:install()
			end
		end
	end)
end

vim.api.nvim_create_user_command("MasonEnsure", ensure_packages_installed, {
	desc = "Refresh the Mason registry and install any missing packages",
})

-- Only refresh/install automatically when something is missing; avoids hitting the network on every startup.
for _, pkg_name in ipairs(packages_to_install) do
	if not registry.is_installed(pkg_name) then
		ensure_packages_installed()
		break
	end
end
