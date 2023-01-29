local null_ls_ok, null_ls = pcall(require, 'null-ls')
if not null_ls_ok then return end

local mason_null_ls_ok, mason_null_ls = pcall(require, 'mason-null-ls')
if not mason_null_ls_ok then return end

local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

local function lsp_formatting(bufnr)
  vim.lsp.buf.format({
    filter = function(client)
      return client.name == "null-ls"
    end,
    bufnr = bufnr,
  })
end

null_ls.setup {
  sources = {
    null_ls.builtins.diagnostics.eslint_d.with({
      diagnostics_format = '[eslint] #{m}\n(#{c})',
      condition = function(utils)
        return utils.root_has_file({
          '.eslintrc',
          '.eslintrc.json',
          '.eslintrc.js',
          '.eslintrc.cjs',
          '.eslintrc.yml',
          '.eslintrc.yaml',
        })
      end
    }),
    null_ls.builtins.code_actions.eslint_d.with({
      condition = function(utils)
        return utils.root_has_file({
          '.eslintrc',
          '.eslintrc.json',
          '.eslintrc.js',
          '.eslintrc.cjs',
          '.eslintrc.yml',
          '.eslintrc.yaml',
        })
      end
    }),
    null_ls.builtins.formatting.eslint_d.with({
      condition = function(utils)
        return utils.root_has_file({
          '.eslintrc',
          '.eslintrc.json',
          '.eslintrc.js',
          '.eslintrc.cjs',
          '.eslintrc.yml',
          '.eslintrc.yaml',
        })
      end
    }),
    null_ls.builtins.formatting.prettierd.with({
      condition = function(utils)
        return utils.root_has_file({
          '.prettierrc',
          '.prettierrc.json',
          '.prettierrc.js',
          '.prettierrc.yml',
          '.prettierrc.yaml',
          '.prettierrc.toml',
        })
      end
    }),
    null_ls.builtins.formatting.rustfmt,
  },
  on_attach = function(client, bufnr)
    if client.supports_method("textDocument/formatting") then
      vim.api.nvim_clear_autocmds({
        group = augroup,
        buffer = bufnr,
      })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = augroup,
        buffer = bufnr,
        callback = function()
          lsp_formatting(bufnr)
        end,
      })
    end
  end
}


-- Mason null-ls plugin must be loaded after
mason_null_ls.setup {
  automatic_installation = true,
}
