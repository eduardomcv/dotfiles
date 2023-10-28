local ok, todo_comments = pcall(require, 'todo-comments')
if not ok then return end

local u = require('eduardomcv.utils')

todo_comments.setup {
  keywords = {
    DELETEME = {
      icon = '',
      color = '#ff0000',
    },
    FIXME = {
      icon = '',
      color = '#fff400',
    },
    HACK = {
      icon = '',
      color = '#00f9ff',
    },
    TODO = {
      icon = '',
      color = '#ed00ff',
    },
    WIP = {
      icon = '',
      color = '#ff7C00',
    }
  },
  merge_keywords = false,
  highlight = {
    pattern = [[.*<(KEYWORDS)\s*]],
  },
  search = {
    pattern = [[\b(KEYWORDS)\b]],
    command = 'rg',
    args = {
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--hidden',
    },
  }
}

u.nmap(
  ']t',
  function()
    todo_comments.jump_next()
  end,
  'Next todo comment'
)

u.nmap(
  ']t',
  function()
    todo_comments.jump_prev()
  end,
  'Previous todo comment'
)

u.nmap('<leader>sc', ':TodoTelescope<CR>')
