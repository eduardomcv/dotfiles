vim.cmd("packadd nvim.undotree")
vim.cmd("packadd nvim.difftool")

vim.keymap.set("n", "<leader>cu", ":Undotree<CR>", { desc = "Toggle undotree" })
