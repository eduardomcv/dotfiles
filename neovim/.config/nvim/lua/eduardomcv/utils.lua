local M = {}

local set = vim.keymap.set

local opts = { silent = true, noremap = true }

M.nmap = function (lhs, rhs)
  set('n', lhs, rhs, opts)
end

M.vmap = function (lhs, rhs)
  set('v', lhs, rhs, opts)
end

return M
