-------- GLOBAL VARIABLES --------
-- Set global variables before
-- loading anything else!

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
----------------------------------

if vim.g.vscode then
	require("vscode_neovim")
else
	require("core")
	require("plugins")
end
