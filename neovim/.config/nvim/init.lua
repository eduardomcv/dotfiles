-------- GLOBAL VARIABLES --------
-- Set global variables before
-- loading anything else!

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
----------------------------------

require("config.options")
require("config.lazy")
require("config.keymaps")
require("config.lsp")
require("config.diagnostic")
