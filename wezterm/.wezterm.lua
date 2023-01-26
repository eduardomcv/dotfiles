local wezterm = require('wezterm')
local act = wezterm.action

return {
  font = wezterm.font 'FiraCode Nerd Font',
  color_scheme = 'tokyonight',
  hide_tab_bar_if_only_one_tab = true,
  window_background_opacity = 0.9,
  use_fancy_tab_bar = false,
  tab_bar_at_bottom = true,
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  keys = {
    { key = 'h', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Left' },
    { key = 'l', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Right' },
    { key = 'k', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Up' },
    { key = 'j', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Down' },
    { key = 'h', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize { 'Left', 1 } },
    { key = 'l', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize { 'Right', 1 } },
    { key = 'k', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize { 'Up', 1 } },
    { key = 'j', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize { 'Down', 1 } },
  }
}
