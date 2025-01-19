local wezterm = require("wezterm")

local config = {
	default_prog = { "tmux", "new", "-A", "-s", "default" },
	font = wezterm.font_with_fallback({ "JetBrains Mono" }),
	color_scheme = "Catppuccin Mocha",
	hide_tab_bar_if_only_one_tab = true,
	font_size = 14,
	window_padding = {
		left = 4,
		right = 4,
		top = 0,
		bottom = 0,
	},
}

-- An util to check which OS wezterm is running on.
local function detect_host_os()
	-- package.config:sub(1,1) returns '\' for windows and '/' for *nix.
	if package.config:sub(1, 1) == "\\" then
		return "windows"
	else
		local result = "unknown"

		-- uname should be available on *nix systems.
		local handler = io.popen("uname -s")

		if handler ~= nil then
			result = handler:read("*l")
			handler:close()
		end

		if result == "unknown" then
			return "unknown"
		elseif result == "Darwin" then
			return "macos"
		else
			return "linux"
		end
	end
end

local host_os = detect_host_os()

-- add homebrew binaries to $PATH on macos
if host_os == "macos" then
	config.set_environment_variables = {
		PATH = "/opt/homebrew/bin:/usr/local/bin:" .. os.getenv("PATH"),
	}
end

return config
