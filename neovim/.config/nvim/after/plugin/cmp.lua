local ok_cmp, cmp = pcall(require, 'cmp')
if not ok_cmp then return end

local ok_lspkind, lspkind = pcall(require, 'lspkind')
if not ok_lspkind then return end

local ok_luasnip, luasnip = pcall(require, 'luasnip')
if not ok_luasnip then return end

local ok_cmp_npm, cmp_npm = pcall(require, 'cmp-npm')
if not ok_cmp_npm then return end

local ok_ts_utils, ts_utils = pcall(require, 'nvim-treesitter.ts_utils')
if not ok_ts_utils then return end

-- Set completeopt to have a better completion experience
vim.opt.completeopt = { 'menuone', 'noinsert', 'noselect' }
-- Avoid showing message extra message when using completion
vim.opt.shortmess:append('c')

-- Load vscode snippets
require("luasnip.loaders.from_vscode").lazy_load()

local function has_words_before()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
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

local function lsp_entry_filter(entry, context)
  local kind = entry:get_kind()
  local node_at_cursor = ts_utils.get_node_at_cursor()

  local line = context.cursor_line
  local col = context.cursor.col
  local char_before_cursor = string.sub(line, col - 1, col - 1)

  if char_before_cursor == '.' then
    return kind == 2 or kind == 5
  elseif string.match(line, '^%s*%w*$') then
    return kind == 3 or kind == 6
  end

  return true
end

cmp_npm.setup {}

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
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete({}),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true
    }),
    ['<Down>'] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }), { 'i' }),
    ['<Up>'] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }), { 'i' }),
    ['<Tab>'] = cmp.mapping(tab, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(shift_tab, { 'i', 's' }),
  },
  sources = cmp.config.sources({
    {
      name = 'nvim_lsp',
      entry_filter = lsp_entry_filter,
    },
    { name = 'luasnip' },
  }, {
    { name = 'npm' },
  }, {
    { name = 'buffer' },
  }),
  formatting = {
    format = lspkind.cmp_format({
      mode = 'symbol_text',
      maxwidth = 40,
      ellipsis_char = '...',
    }),
  },
})
