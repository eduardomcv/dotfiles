local ok, copilot = pcall(require, 'copilot')
if not ok then return end

copilot.setup {
  suggestion = {
    auto_trigger = true,
    keymap = {
      accept = '<C-j>',
    },
  },
  filetypes = {
    ['*'] = false,
    javascript = true,
    typescript = true,
    typescriptreact = true
  }
}
