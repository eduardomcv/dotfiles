-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

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

-- Set <c-p> as file picker.
set("n", "<c-p>", LazyVim.pick("files"), { desc = "Find Files (Root Dir)" })
