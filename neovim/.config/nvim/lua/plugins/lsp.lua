vim.pack.add({
	-- Collection of LSP server configurations (good defaults)
	"https://github.com/neovim/nvim-lspconfig",
})

vim.lsp.enable({
	"bashls",
	"lua_ls",
	"jsonls",
	"yamlls",
	"vtsls",
	"eslint",
	"oxlint",
	"cssls",
	"html",
	"emmet_language_server",
	"ty",
	"ruff",
	"copilot",
})

--- LSP server configurations

vim.lsp.config("vtsls", {
	settings = {
		complete_function_calls = true,
		vtsls = {
			enableMoveToFileCodeAction = true,
			autoUseWorkspaceTsdk = true,
			maxTsServerMemory = 8192,
			experimental = {
				completion = {
					enableServerSideFuzzyMatch = true,
				},
			},
		},
		typescript = {
			updateImportsOnFileMove = { enabled = "prompt" },
			suggest = {
				completeFunctionCalls = true,
			},
			preferences = {
				-- Enable this only when needed. This is a memory hog.
				-- includePackageJsonAutoImports = "on",
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

local ruff_base_on_attach = vim.lsp.config.ruff.on_attach
vim.lsp.config("ruff", {
	cmd = function(dispatchers, config)
		local ruff_bin = "ruff"
		local root = config.root_dir or vim.fn.getcwd()
		local venv = vim.fs.root(root, ".venv")
		local venv_ruff = venv and (venv .. "/bin/ruff") or nil

		if venv_ruff ~= nil and vim.fn.executable(venv_ruff) == 1 then
			ruff_bin = venv_ruff
		end

		return vim.lsp.rpc.start({ ruff_bin, "server" }, dispatchers, { cwd = config.cmd_cwd })
	end,
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

		-- Disable hover in favor of ty
		client.server_capabilities.hoverProvider = false
	end,
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
			group = vim.api.nvim_create_augroup("eslint-fix-all-" .. bufnr, { clear = true }),
			buffer = bufnr,
			command = "LspEslintFixAll",
		})
	end,
})

vim.lsp.config("copilot", {
	settings = {
		telemetry = {
			telemetryLevel = "off",
		},
	},
})

--- Autocmds

-- Enable lsp-inline-completion if supported (such as for the copilot language server)
vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("lsp-inline-completion", { clear = true }),
	callback = function(args)
		local bufnr = args.buf
		local client = assert(vim.lsp.get_client_by_id(args.data.client_id))

		if client:supports_method(vim.lsp.protocol.Methods.textDocument_inlineCompletion, bufnr) then
			vim.lsp.inline_completion.enable(true, { bufnr = bufnr })

			vim.keymap.set("i", "<Tab>", function()
				if not vim.lsp.inline_completion.get() then
					return "<Tab>"
				end
			end, {
				expr = true,
				desc = "LSP: accept inline completion",
				buffer = bufnr,
			})

			vim.keymap.set("i", "<c-tab>", vim.lsp.inline_completion.select, {
				desc = "LSP: switch inline completion",
				buffer = bufnr,
			})
		end
	end,
})

--- Keymaps

vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, { desc = "Code Action" })
vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Rename" })
vim.keymap.set({ "n", "v" }, "<leader>cl", vim.lsp.codelens.run, { desc = "Run Codelens" })
vim.keymap.set("n", "<leader>cL", function()
	vim.lsp.codelens.enable(true)
end, { desc = "Refresh & Display Codelens" })
