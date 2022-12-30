local utils = require('user.utils')

-- Toggle undotree
utils.nmap('<leader>u', ':UndotreeToggle<CR>:UndotreeFocus<CR>')
