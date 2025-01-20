local set = vim.keymap.set

-- Delete without yank
set("n", "x", [["_x]], { desc = "Delete without yanking" })
set({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete without yanking" })

-- Increment/decrement
set("n", "+", "<C-a>")
set("n", "-", "<C-x>")

-- Tab creation
set("n", "<leader>tn", "<cmd>tabnew<cr>", { desc = "Create new tab" })
set("n", "<leader>tc", "<cmd>tabclose<cr>", { desc = "Close tab" })

-- Cycle tabs with [t and ]t
set("n", "[t", "<cmd>tabprev<cr>", { desc = "Previous tab" })
set("n", "]t", "<cmd>tabnext<cr>", { desc = "Next tab" })
