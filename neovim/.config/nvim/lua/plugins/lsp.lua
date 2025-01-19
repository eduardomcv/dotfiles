return {
	{
		"williamboman/mason-lspconfig.nvim",
		dependencies = "mason.nvim",
		config = function() end,
	},
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			"mason-lspconfig.nvim",
			"blink.cmp",
		},
		config = function()
			-- A map of server names to configurations
			local servers = {
				lua_ls = {
					settings = {
						Lua = {
							workspace = {
								checkThirdParty = false,
							},
							codeLens = {
								enable = true,
							},
							completion = {
								callSnippet = "Replace",
							},
							hint = {
								enable = true,
							},
						},
					},
				},
				vtsls = {
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
						},
						javascript = {
							updateImportsOnFileMove = { enabled = "always" },
							suggest = {
								completeFunctionCalls = true,
							},
						},
					},
				},
				eslint = {
					workingDirectories = { mode = "auto" },
					format = true,
				},
			}

			-- Automatically install LSP servers listed in servers
			local ensure_installed = {}
			for server, _ in pairs(servers) do
				ensure_installed[#ensure_installed + 1] = server
			end

			-- This is the default function used by mason-lspconfig to setup LSP servers.
			local function setup_server(server)
				local config = servers[server] or {}
				-- Setup blink.cmp capabilities
				config.capabilities = require("blink.cmp").get_lsp_capabilities(config.capabilities)
				-- Setup lsp server config
				require("lspconfig")[server].setup(config)
			end

			require("mason-lspconfig").setup({
				automatic_installation = true,
				ensure_installed = ensure_installed,
				handlers = {
					setup_server,
					-- Create specific config for eslint for setting up an autocmd
					-- to fix lint issues on save
					eslint = function()
						require("lspconfig").eslint.setup({
							on_attach = function(_, bufnr)
								vim.api.nvim_create_autocmd("BufWritePre", {
									group = vim.api.nvim_create_augroup("eslint-lsp", { clear = true }),
									buffer = bufnr,
									command = "EslintFixAll",
								})
							end,
						})
					end,
				},
			})

			-- Configure diagnostics
			vim.diagnostic.config({
				underline = true,
				severity_sort = true,
				update_in_insert = false,
				virtual_text = {
					spacing = 4,
					source = "if_many",
					prefix = "●",
				},
				signs = {
					text = {
						[vim.diagnostic.severity.ERROR] = " ",
						[vim.diagnostic.severity.WARN] = " ",
						[vim.diagnostic.severity.HINT] = " ",
						[vim.diagnostic.severity.INFO] = " ",
					},
				},
			})

			-- Setup keymaps (setting this in "keys" breaks the plugin)
			vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Goto definition" })
			vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { desc = "Goto Declaration" })
			vim.keymap.set("n", "gr", vim.lsp.buf.references, { desc = "References", nowait = true })
			vim.keymap.set("n", "gI", vim.lsp.buf.implementation, { desc = "Goto Implementation" })
			vim.keymap.set("n", "gy", vim.lsp.buf.type_definition, { desc = "Goto Type Definition" })
			vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, { desc = "Code Action" })
			vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Rename" })
			vim.keymap.set({ "n", "v" }, "<leader>cl", vim.lsp.codelens.run, { desc = "Run Codelens" })
			vim.keymap.set("n", "<leader>cL", vim.lsp.codelens.refresh, { desc = "Refresh & Display Codelens" })
			vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Goto previous diagnostic" })
			vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Goto next diagnostic" })
		end,
	},
}
