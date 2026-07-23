-- VSCode-compatible options.
-- Many options are redundant, since they are handled by VSCode itself (line numbers, scrolloff, etc.).
-- Only set what's necessary.

local o = vim.opt

-- Use system clipboard
o.clipboard:append({ "unnamedplus" })

-- Persistent undo
o.undofile = true
o.undolevels = 10000

-- Don't use swapfile
o.swapfile = false

-- Keep conceallevel consistent
o.conceallevel = 1
