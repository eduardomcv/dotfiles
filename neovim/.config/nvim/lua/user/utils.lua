local M = {}

local set = vim.keymap.set

M.nmap = function(lhs, rhs, desc)
  set('n', lhs, rhs, { silent = true, noremap = true, desc = desc })
end

M.vmap = function(lhs, rhs, desc)
  set('v', lhs, rhs, { silent = true, noremap = true, desc = desc })
end

return M
