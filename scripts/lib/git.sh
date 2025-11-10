#!/bin/bash

set_user_git_info() {
	# Prompt the user for the git username and email
	echo -n "Enter full name (for git): "
	IFS="\n" read -r GIT_USERNAME

	echo -n "Enter email (for git): "
	IFS="\n" read -r GIT_EMAIL

	git config --global user.name "$GIT_USERNAME"
	git config --global user.email "$GIT_EMAIL"
}
