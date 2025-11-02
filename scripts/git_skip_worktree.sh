#!/bin/bash

# This script makes git skip changes in the files listed in $FILES_TO_SKIP

set -euo pipefail

# All scripts and sources are located in reference to the root of this repo.
REPO_ROOT="$(git rev-parse --show-toplevel)"

# Skip by default
UPDATE_INDEX_ARG="--skip-worktree"

FILES_TO_SKIP=(
	"zsh/.zshrc"
	"git/.gitconfig"
)

if [[ "$#" > 0 ]]; then
	if [[ "$1" == "unskip" || "$1" == "noskip" ]]; then
		# Undo skipping if any of the above is passed
		UPDATE_INDEX_ARG="--no-skip-worktree"
	fi
fi

for FILE in ${FILES_TO_SKIP[@]}; do
	git update-index "$UPDATE_INDEX_ARG" "$REPO_ROOT/$FILE"
done
