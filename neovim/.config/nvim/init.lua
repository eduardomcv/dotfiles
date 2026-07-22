-------- GLOBAL VARIABLES --------
-- Set global variables before
-- loading anything else!

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
----------------------------------

require("core")

if not vim.g.vscode then
	require("plugins")
end
