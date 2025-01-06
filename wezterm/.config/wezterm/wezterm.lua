local wezterm = require("wezterm")

local config = {
	default_prog = { "tmux", "new", "-A", "-s", "default" },
	font = wezterm.font_with_fallback({ "JetBrains Mono" }),
	color_scheme = "Catppuccin Mocha",
	hide_tab_bar_if_only_one_tab = true,
	font_size = 12,
	window_padding = {
		left = 4,
		right = 4,
		top = 4,
		bottom = 4,
	},
}

return config
