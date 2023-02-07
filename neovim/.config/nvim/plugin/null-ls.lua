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

local function package_json_has_keyword(keyword)
  local file_path = require('null-ls.utils').get_root() .. "/package.json"
  local file = io.open(file_path)

  if file == nil then
    return false
  end

  local file_contents = file:read("*a")
  file:close()

  local has_keyword = string.find(file_contents, keyword) ~= nil
  return has_keyword
end

null_ls.setup {
    sources = {
        null_ls.builtins.diagnostics.eslint_d.with({
            diagnostics_format = '[eslint] #{m}\n(#{c})',
            condition = function(utils)
              local has_config = utils.root_has_file({
                      '.eslintrc',
                      '.eslintrc.json',
                      '.eslintrc.js',
                      '.eslintrc.cjs',
                      '.eslintrc.yml',
                      '.eslintrc.yaml',
                  })

              return has_config or package_json_has_keyword("eslint")
            end
        }),
        null_ls.builtins.diagnostics.standardjs.with({
            condition = function()
              return package_json_has_keyword("standard")
            end,
        }),
        null_ls.builtins.code_actions.eslint_d.with({
            condition = function(utils)
              local has_config = utils.root_has_file({
                      '.eslintrc',
                      '.eslintrc.json',
                      '.eslintrc.js',
                      '.eslintrc.cjs',
                      '.eslintrc.yml',
                      '.eslintrc.yaml',
                  })

              return has_config or package_json_has_keyword("eslint")
            end
        }),
        null_ls.builtins.formatting.eslint_d.with({
            condition = function(utils)
              local has_config = utils.root_has_file({
                      '.eslintrc',
                      '.eslintrc.json',
                      '.eslintrc.js',
                      '.eslintrc.cjs',
                      '.eslintrc.yml',
                      '.eslintrc.yaml',
                  })

              return has_config or package_json_has_keyword("eslint")
            end
        }),
        null_ls.builtins.formatting.prettierd.with({
            condition = function(utils)
              local has_config = utils.root_has_file({
                      '.prettierrc',
                      '.prettierrc.json',
                      '.prettierrc.js',
                      '.prettierrc.yml',
                      '.prettierrc.yaml',
                      '.prettierrc.toml',
                  })

              return has_config or package_json_has_keyword("prettier")
            end
        }),
        null_ls.builtins.formatting.standardjs.with({
            condition = function()
              return package_json_has_keyword("standard")
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
