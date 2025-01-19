-------- GLOBAL VARIABLES --------
-- Set global variables before
-- loading anything else!

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.g.obsidian_vault_dir = vim.fn.expand("~") .. "/Obsidian/Vault 22/"
----------------------------------

require("config.options")
require("config.lazy")
require("config.keymaps")
