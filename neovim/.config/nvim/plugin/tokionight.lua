local ok, tokyonight = pcall(require, 'tokyonight')
if not ok then return end

tokyonight.setup {
  style = 'night',
  styles = {
    comments = { italic = false },
    keywords = { italic = false },
  },
}

vim.cmd [[colorscheme tokyonight]]
