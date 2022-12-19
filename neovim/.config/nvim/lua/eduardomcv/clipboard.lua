local has = vim.fn.has

if has "unix" == 1 then
  vim.opt.clipboard:append { 'unnamedplus' }
end

if has "wsl" == 1 then
  vim.g.clipboard = {
    name = "win32yank-wsl",
    copy = {
      ["+"] = "win32yank.exe -i",
      ["*"] = "win32yank.exe -i",
    },
    paste = {
      ["+"] = "win32yank.exe -o",
      ["*"] = "win32yank.exe -o",
    },
    cache_enabled = 0,
  }
end

if has "win32" == 1 then
  vim.opt.clipboard:prepend { 'unnamed', 'unnamedplus' }
end
