local wezterm = require("wezterm")

local config = {
	default_prog = { "tmux" },
	font = wezterm.font_with_fallback({
		"FiraCode Nerd Font",
		"JetBrains Mono",
	}),
	color_scheme = "Catppuccin Mocha",
	hide_tab_bar_if_only_one_tab = true,
	font_size = 12,
	window_padding = {
		left = 0,
		right = 0,
		top = 2,
		bottom = 0,
	},
}

return config
