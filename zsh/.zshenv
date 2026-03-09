# Exports
export EDITOR="nvim"
export VISUAL="nvim"

# Add the user's private bin to PATH if it exists
local_bin_dir="$HOME/.local/bin"
if [[ -d $local_bin_dir ]]; then
    path=("$local_bin_dir" $path)
fi

