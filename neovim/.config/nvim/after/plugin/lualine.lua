local ok, lualine = pcall(require, "lualine")
if not ok then return end

lualine.setup {
  options = {
    icons_enabled = true,
    theme = 'vscode',
    component_separators = {
      left = '',
      right = '',
    },
    section_separators = {
      left = '',
      right = '',
    },
    disabled_filetypes = {},
    always_divide_middle = true,
  },
  sections = {
    lualine_a = { 'mode' },
    lualine_b = {
      'branch',
      'diff',
    },
    lualine_c = {
      {
        'filename',
        file_status = true, -- displays file status (readonly status, modified status)
        path = 0, -- 0 = just filename, 1 = relative path, 2 = absolute path
      },
      'aerial',
    },
    lualine_x = {
      {
        'diagnostics',
        sources = { 'nvim_diagnostic' },
        symbols = {
          error = ' ',
          warn = ' ',
          info = ' ',
          hint = ' '
        }
      },
      'encoding',
      'fileformat',
      'filetype',
    },
    lualine_y = { 'progress' },
    lualine_z = { 'location' }
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { {
      'filename',
      file_status = true, -- displays file status (readonly status, modified status)
      path = 1, -- 0 = just filename, 1 = relative path, 2 = absolute path
    } },
    lualine_x = { 'location' },
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = { 'fugitive' }
}
