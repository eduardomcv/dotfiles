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
