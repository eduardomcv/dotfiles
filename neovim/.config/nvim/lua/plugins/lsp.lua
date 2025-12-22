vim.pack.add({
	--- Default configurations for lsp
	"https://github.com/neovim/nvim-lspconfig",
})

vim.lsp.enable({
	"basedpyright",
	"vtsls",
	"lua_ls",
	"cssls",
	"html",
	"jsonls",
	"yamlls",
	"eslint",
	"ruff",
})

vim.lsp.config("vtsls", {
	settings = {
		complete_function_calls = true,
		vtsls = {
			enableMoveToFileCodeAction = true,
			autoUseWorkspaceTsdk = true,
			experimental = {
				completion = {
					enableServerSideFuzzyMatch = true,
				},
			},
		},
		typescript = {
			updateImportsOnFileMove = { enabled = "always" },
			suggest = {
				completeFunctionCalls = true,
			},
			preferences = {
				includePackageJsonAutoImports = "on",
			},
		},
		javascript = {
			updateImportsOnFileMove = { enabled = "always" },
			suggest = {
				completeFunctionCalls = true,
			},
		},
	},
})

vim.lsp.config("lua_ls", {
	on_init = function(client)
		if client.workspace_folders then
			local path = client.workspace_folders[1].name
			if
				path ~= vim.fn.stdpath("config")
				and (vim.uv.fs_stat(path .. "/.luarc.json") or vim.uv.fs_stat(path .. "/.luarc.jsonc"))
			then
				return
			end
		end

		client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
			runtime = {
				-- Tell the language server which version of Lua you're using (most
				-- likely LuaJIT in the case of Neovim)
				version = "LuaJIT",
				-- Tell the language server how to find Lua modules same way as Neovim
				-- (see `:h lua-module-load`)
				path = {
					"lua/?.lua",
					"lua/?/init.lua",
				},
			},
			-- Make the server aware of Neovim runtime files
			workspace = {
				checkThirdParty = false,
				library = {
					vim.env.VIMRUNTIME,
					-- Depending on the usage, you might want to add additional paths
					-- here.
					-- '${3rd}/luv/library'
					-- '${3rd}/busted/library'
				},
				-- Or pull in all of 'runtimepath'.
				-- NOTE: this is a lot slower and will cause issues when working on
				-- your own configuration.
				-- See https://github.com/neovim/nvim-lspconfig/issues/3189
				-- library = {
				--   vim.api.nvim_get_runtime_file('', true),
				-- }
			},
		})
	end,
	settings = {
		Lua = {},
	},
})

local ruff_base_on_attach = vim.lsp.config.ruff.on_attach
vim.lsp.config("ruff", {
	init_options = {
		settings = {
			-- Prioritize project config over editor config
			configurationPreference = "filesystemFirst",
		},
	},
	on_attach = function(client, bufnr)
		if ruff_base_on_attach ~= nil then
			ruff_base_on_attach(client, bufnr)
		end

		-- Disable hover in favor of pyright
		client.server_capabilities.hoverProvider = false
	end,
})

vim.lsp.config("basedpyright", {
	settings = {
		basedpyright = {
			-- Using Ruff's import organizer
			disableOrganizeImports = true,
			analysis = {
				-- Ignore all files for analysis to exclusively use Ruff for linting
				ignore = { "*" },
			},
		},
	},
})

local eslint_base_on_attach = vim.lsp.config.eslint.on_attach
vim.lsp.config("eslint", {
	on_attach = function(client, bufnr)
		if not eslint_base_on_attach then
			-- The base on_attach provides the LspEslintFixAll command
			return
		end

		eslint_base_on_attach(client, bufnr)

		vim.api.nvim_create_autocmd("BufWritePre", {
			buffer = bufnr,
			command = "LspEslintFixAll",
		})
	end,
})

--- Keymaps
vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, { desc = "Code Action" })
vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Rename" })
vim.keymap.set({ "n", "v" }, "<leader>cl", vim.lsp.codelens.run, { desc = "Run Codelens" })
vim.keymap.set("n", "<leader>cL", vim.lsp.codelens.refresh, { desc = "Refresh & Display Codelens" })
