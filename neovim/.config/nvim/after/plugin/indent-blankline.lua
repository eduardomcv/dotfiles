local ok, indent_blankline = pcall(require, 'indent_blankline')
if not ok then return end

vim.opt.list = true
vim.opt.listchars:append("space:⋅")

indent_blankline.setup {
  space_char_blankline = " ",
  show_end_of_line = false,
  show_current_context = true,
}
