vim.pack.add({
	--- Default configurations for lsp
	"https://github.com/neovim/nvim-lspconfig",
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

--- Keymaps
vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, { desc = "Code Action" })
vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Rename" })
vim.keymap.set({ "n", "v" }, "<leader>cl", vim.lsp.codelens.run, { desc = "Run Codelens" })
vim.keymap.set("n", "<leader>cL", vim.lsp.codelens.refresh, { desc = "Refresh & Display Codelens" })

--- Autocmds
vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("lsp_attach_disable_ruff_hover", { clear = true }),
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		if client == nil then
			return
		end
		if client.name == "ruff" then
			-- Disable hover in favor of python-lsp-server
			client.server_capabilities.hoverProvider = false
		end
	end,
	desc = "LSP: Disable hover capability from Ruff",
})
