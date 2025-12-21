# Exports
export EDITOR=nvim
export VISUAL=nvim

# Add the user's private bin to PATH if it exists
local_bin_dir="$HOME/.local/bin"
if [ -d $local_bin_dir ]; then
	path+=("$local_bin_dir")
fi

# Add the emacs bin to PATH if it exists
emacs_bin_dir="$HOME/.config/emacs/bin"
if [ -d $emacs_bin_dir ]; then
	path+=("$emacs_bin_dir")
fi
