vim.cmd("packadd nvim.undotree")
vim.cmd("packadd nvim.difftool")

vim.keymap.set("n", "<leader>u", ":Undotree<CR>", { desc = "Toggle undotree" })
