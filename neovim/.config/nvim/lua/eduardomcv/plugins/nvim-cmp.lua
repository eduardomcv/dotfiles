-- Completion plugin

return {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    "hrsh7th/cmp-buffer", -- nvim-cmp source for words within buffer
    "hrsh7th/cmp-path", -- nvim-cmp source for paths
    "L3MON4D3/LuaSnip", -- Snippet engine
    "saadparwaiz1/cmp_luasnip", -- Luasnip completion source for nvim-cmp
    "onsails/lspkind.nvim", -- vscode-like pictograms
    "rafamadriz/friendly-snippets", -- vscode-like snippets
    "nvim-autopairs", -- Auto-pair plugin
    {
      "David-Kunz/cmp-npm", -- nvim-cmp source for npm
      dependencies = {
        "nvim-lua/plenary.nvim",
      },
    },
  },
  config = function()
    local cmp = require("cmp")
    local luasnip = require("luasnip")
    local cmp_autopairs = require("nvim-autopairs.completion.cmp")

    cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

    -- Load vscode snippets
    require("luasnip.loaders.from_vscode").lazy_load()

    -- Load npm completion
    require("cmp-npm").setup()

    local function has_words_before()
      local line, col = unpack(vim.api.nvim_win_get_cursor(0))
      return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
    end

    local function tab(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      elseif has_words_before() then
        cmp.complete()
      else
        fallback()
      end
    end

    local function shift_tab(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end

    cmp.setup({
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      window = {
        completion = cmp.config.window.bordered({ side_padding = 0 }),
        documentation = cmp.config.window.bordered(),
      },
      mapping = {
        ["<C-d>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete({}),
        ["<C-e>"] = cmp.mapping.close(),
        ["<CR>"] = cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        }),
        ["<Down>"] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }), { "i" }),
        ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }), { "i" }),
        ["<Tab>"] = cmp.mapping(tab, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(shift_tab, { "i", "s" }),
      },
      sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "luasnip" },
        { name = "path" },
      }, {
        { name = "npm" },
      }, {
        { name = "buffer" },
      }),
      formatting = {
        format = require("lspkind").cmp_format({
          mode = "symbol_text",
          maxwidth = 40,
          ellipsis_char = "...",
        }),
      },
    })

    -- Set completeopt to have a better completion experience
    vim.opt.completeopt = { "menuone", "noinsert", "noselect" }
    -- Avoid showing message extra message when using completion
    vim.opt.shortmess:append("c")
  end,
}
