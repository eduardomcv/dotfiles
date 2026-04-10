local kset = vim.keymap.set

-- Delete without yank
kset("n", "x", [["_x]], { desc = "Delete without yanking" })
kset({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete without yanking" })

-- Increment/decrement
kset("n", "+", "<C-a>")
kset("n", "-", "<C-x>")

-- Tab creation
kset("n", "<leader>tn", "<cmd>tabnew<cr>", { desc = "Create new tab" })
kset("n", "<leader>tc", "<cmd>tabclose<cr>", { desc = "Close tab" })

-- Open command line in lua mode
kset("n", "<leader>:", ":lua ", { desc = "Open command line in lua mode" })

-- Window navigation
kset("n", "<c-h>", "<c-w>h", { desc = "Go to the left window" })
kset("n", "<c-j>", "<c-w>j", { desc = "Go to the down window" })
kset("n", "<c-k>", "<c-w>k", { desc = "Go to the up window" })
kset("n", "<c-l>", "<c-w>l", { desc = "Go to the right window" })
