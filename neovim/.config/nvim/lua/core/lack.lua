vim.pack.add({ "https://github.com/eduardomcv/lack.nvim" })

local lack = require("lack")

lack.setup()
---@type lack.Module
_G.use = lack
