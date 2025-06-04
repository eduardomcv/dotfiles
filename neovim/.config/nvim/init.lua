-------- GLOBAL VARIABLES --------
-- Set global variables before
-- loading anything else!

IS_VSCODE = vim.g.vscode ~= nil

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
----------------------------------

require("config.options")
require("config.lazy")
require("config.keymaps")
