local ok, lspconfig = pcall(require, "lspconfig")
if not ok then return end

-- Global mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
-- vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
-- vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    -- vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    -- vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
    -- vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
    -- vim.keymap.set('n', '<leader>wl', function()
    --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    -- end, opts)
    vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<leader>f', function()
      vim.lsp.buf.format { async = true }
    end, opts)
  end,
})

-- Set up completion using nvim_cmp with LSP source
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Completion icons
vim.lsp.protocol.CompletionItemKind = {
  '', -- Text
  '', -- Method
  '', -- Function
  '', -- Constructor
  '', -- Field
  '', -- Variable
  '', -- Class
  'ﰮ', -- Interface
  '', -- Module
  '', -- Property
  '', -- Unit
  '', -- Value
  '', -- Enum
  '', -- Keyword
  '﬌', -- Snippet
  '', -- Color
  '', -- File
  '', -- Reference
  '', -- Folder
  '', -- EnumMember
  '', -- Constant
  '', -- Struct
  '', -- Event
  'ﬦ', -- Operator
  '', -- TypeParameter
}

-- icon for diagnostics
vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics,
  {
    underline = true,
    virtual_text = {
      spacing = 4,
      prefix = '●',
    }
  }
)

-- Borders for hover/float windows
vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(
  vim.lsp.handlers.hover,
  { border = 'rounded' }
)

vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(
  vim.lsp.handlers.signature_help,
  { border = 'rounded' }
)

-- Diagnostic symbols in the sign column (gutter)
local signs = {
  Error = " ",
  Warn = " ",
  Hint = " ",
  Info = " ",
}

for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, {
    text = icon,
    texthl = hl,
    numhl = hl,
  })
end

vim.diagnostic.config({
  virtual_text = {
    prefix = '●'
  },
  update_in_insert = true,
  float = {
    source = "always",  -- Or "if_many"
    border = 'rounded', -- Rounded border
  },
})

-- Server configurations
local servers = {
  cssls = {},
  html = {},
  jsonls = {},
  emmet_ls = {},
  eslint = {},
  rust_analyzer = {},
  graphql = {},
  tsserver = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
      diagnostics = {
        globals = { 'vim' },
      },
    },
  }
}

-- neodev must be loaded before lspconfig
require('neodev').setup {
  library = {
    plugins = {
      'neotest',
    },
    types = true,
  },
}

-- Mason must be loaded before lspconfig
require('mason').setup {}

require('mason-lspconfig').setup {
  automatic_installation = true,
  ensure_installed = vim.tbl_keys(servers),
}

require('mason-lspconfig').setup_handlers {
  function(server_name)
    lspconfig[server_name].setup {
      capabilities = capabilities,
      settings = servers[server_name],
    }
  end,
  graphql = function()
    lspconfig.graphql.setup {
      on_attach = function(client)
        client.server_capabilities.hoverProvider = false
      end,
      capabilities = capabilities,
      settings = servers.graphql,
    }
  end,
  tsserver = function()
    require('typescript').setup {
      server = {
        capabilities = capabilities,
        settings = servers.tsserver,
        init_options = {
          plugins = {
            {
              name = 'typescript-styled-plugin',
              location = '/Users/eduardo/.nvm/versions/node/v16.19.1/lib' -- Path to global node modules install location
            },
          },
        },
      }
    }
  end,
  eslint = function()
    lspconfig.eslint.setup {
      capabilities = capabilities,
      settings = servers.eslint,
      on_attach = function(_, bufnr)
        -- Fix on save
        vim.api.nvim_create_autocmd("BufWritePre", {
          buffer = bufnr,
          command = "EslintFixAll",
        })
      end
    }
  end,
  lua_ls = function()
    lspconfig.lua_ls.setup {
      on_attach = function(_, bufnr)
        -- Format on save
        local augroup_format = vim.api.nvim_create_augroup("Format", { clear = true })

        vim.api.nvim_clear_autocmds({
          group = augroup_format,
          buffer = bufnr
        })

        vim.api.nvim_create_autocmd("BufWritePre", {
          group = augroup_format,
          buffer = bufnr,
          callback = function()
            vim.lsp.buf.format({ bufnr = bufnr })
          end,
        })
      end,
      capabilities = capabilities,
      settings = servers.lua_ls,
    }
  end
}
