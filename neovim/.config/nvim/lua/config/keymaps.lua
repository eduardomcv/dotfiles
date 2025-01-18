local set = vim.keymap.set

-- Delete without yank
set("n", "x", [["_x]], { desc = "Delete without yanking" })
set({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete without yanking" })

-- Increment/decrement
set("n", "+", "<C-a>")
set("n", "-", "<C-x>")
