local has = vim.fn.has

if has "unix" then
  vim.opt.clipboard:append { 'unnamedplus' }
end

if has "win32" then
  vim.opt.clipboard:prepend { 'unnamed', 'unnamedplus' }
end
