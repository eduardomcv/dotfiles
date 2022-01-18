" show hover doc
nnoremap <silent> K <cmd>lua require('lspsaga.hover').render_hover_doc()<CR>
" scroll down hover doc or scroll in definition preview
nnoremap <silent> <C-f> <cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>
" scroll up hover doc
nnoremap <silent> <C-b> <cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>
" lsp provider to find the cursor word definition and reference
nnoremap <silent> gh :Lspsaga lsp_finder<CR>
" preview definition
nnoremap <silent> gp :Lspsaga preview_definition<CR>
" show signature help
nnoremap <silent> gs <cmd>lua require('lspsaga.signaturehelp').signature_help()<CR>
" jump diagnostics
nnoremap <silent> [d :Lspsaga diagnostic_jump_prev<CR>
nnoremap <silent> ]d :Lspsaga diagnostic_jump_next<CR>

lua << EOF
local saga = require('lspsaga')

saga.init_lsp_saga()
EOF
