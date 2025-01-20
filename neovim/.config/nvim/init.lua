-------- GLOBAL VARIABLES --------
-- Set global variables before
-- loading anything else!

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.g.obsidian_vault_dir = vim.fn.expand("~") .. "/Obsidian/obsidian-vault/"
----------------------------------

require("config.options")
require("config.lazy")
require("config.keymaps")
