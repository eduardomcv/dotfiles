function set_user_git_info() {
	local git_username=""
	local git_email=""

	echo -n "(git config) Enter full name: "
	IFS="\n" read -r git_username

	echo -n "(git config) Enter email: "
	IFS="\n" read -r git_email

	git config --global user.name "$git_username"
	git config --global user.email "$git_email"
}
