local utils = require('eduardomcv.utils')

-- Toggle undotree
utils.nmap('<leader>u', ':UndotreeToggle<CR>:UndotreeFocus<CR>')
