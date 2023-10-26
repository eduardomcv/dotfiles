local ok, ibl = pcall(require, 'ibl')
if not ok then return end

vim.opt.list = true
vim.opt.listchars:append("space:⋅")

ibl.setup()
