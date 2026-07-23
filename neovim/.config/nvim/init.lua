-------- GLOBAL VARIABLES --------
-- Set global variables before
-- loading anything else!

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
----------------------------------

require("core")

if vim.g.vscode then
	require("vscode_neovim")
else
	require("plugins")
end
