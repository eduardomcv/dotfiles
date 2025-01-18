local set = vim.keymap.set

-- Delete without yank
set("n", "x", [["_x]], { desc = "Delete without yanking" })
set({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete without yanking" })

-- Keep cursor in place when joining lines
set("n", "J", "mzJ`z")

-- Keep cursor in the middle when navigating
set("n", "<C-d>", "<C-d>zz")
set("n", "<C-u>", "<C-u>zz")

-- Keep cursor in the middle when repeating searches
set("n", "n", "nzzzv")
set("n", "N", "Nzzzv")

-- Increment/decrement
set("n", "+", "<C-a>")
set("n", "-", "<C-x>")
